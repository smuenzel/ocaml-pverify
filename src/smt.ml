open! Core

module Smt_int = struct
  let size_in_bits = 64

  let of_int i =
    let i64 = Int64.of_int i in
    let bits =
      List.init size_in_bits
        ~f:(fun index ->
            if Int64.(i64 land (one lsl index) = zero)
            then '0'
            else '1'
          )
      |> List.rev
      |> String.of_char_list
    in
    "#b" ^ bits

  let%expect_test "" =
    of_int 0 |> print_endline;
    [%expect {| #b0000000000000000000000000000000000000000000000000000000000000000 |}];
    of_int 1 |> print_endline;
    [%expect {| #b0000000000000000000000000000000000000000000000000000000000000001 |}];
    of_int (-1) |> print_endline;
    [%expect {| #b1111111111111111111111111111111111111111111111111111111111111111 |}];
  ;;

end

module Compare = struct
  type t = Spec.Binary_op.Kind.Compare.t [@@deriving sexp]
end

module Ocaml_op2 = struct
  module Kind = struct
    type t =
      | And
      | Asr
      | Lsl
      | Lsr
      | Or
      | Xor
      | Cmp of ([`Cmp], Compare.t) Spec.Or_variable.t
      | Addi

    let smt_op2 name p1 p2 : Sexp.t =
      List
        [ Atom name
        ; p1
        ; p2
        ]

    let smt_of_t = function
      | And -> smt_op2 "ocaml-and"
      | Asr -> smt_op2 "ocaml-asr"
      | Lsl -> smt_op2 "ocaml-lsl"
      | Lsr -> smt_op2 "ocaml-lsr"
      | Or  -> smt_op2 "ocaml-or"
      | Xor -> smt_op2 "ocaml-xor"
      | Addi -> smt_op2 "ocaml-addi"
      | Cmp (K cmp) -> 
        fun p1 p2 ->
          List
            [ Atom "ocaml-cmp"
            ; [%sexp_of: Compare.t] cmp
            ; p1
            ; p2
            ]
      | Cmp (V cmp) -> 
        fun p1 p2 ->
          List
            [ Atom "ocaml-cmp"
            ; Atom (Spec.Var.Name.to_string cmp.name)
            ; p1
            ; p2
            ]

  end

  type 'param t =
    { kind : Kind.t
    ; p1 : 'param
    ; p2 : 'param
    }

  let smt_of_t smt_of_param t =
    let { kind; p1; p2 } = t in
    Kind.smt_of_t kind
      (smt_of_param p1)
      (smt_of_param p2)
end

module Var =
  (val String_id.make
      ~module_name:"Smt.Var"
      ~include_default_validation:true
      ~caller_identity:String_id.legacy_identity
      ()
  )

module Const = struct
  type t =
    | Zero
    | One
    | Three
    | Neg_one
    | Int of int

  let smt_of_t : t -> Sexp.t = function
    | Zero -> Atom (Smt_int.of_int 0)
    | One -> Atom (Smt_int.of_int 1)
    | Three -> Atom (Smt_int.of_int 3)
    | Neg_one -> Atom (Smt_int.of_int (-1))
    | Int i -> Atom (Smt_int.of_int i)
end

module Expression = struct
  type t =
    | Const of Const.t
    | Var of Var.t
    | Ocaml_op2 of t Ocaml_op2.t

  let rec smt_of_t = function
    | Const const -> Const.smt_of_t const
    | Var var -> Atom (Var.to_string var)
    | Ocaml_op2 op2 ->
      Ocaml_op2.smt_of_t smt_of_t op2

end

module Typ = struct
  type t =
    | Ocaml_int
    | Ocaml_cmp
end

type phrase =
  | Push of int
  | Pop of int
  | Check_sat
  | Get_model
  | Assert of assertion
  | Assert_not of assertion
  | Echo of string
  | Declare_const of { name : string; typ : string }
and assertion =
  | Equal of Expression.t * Expression.t
  | Signed_cmp of Compare.t * Expression.t * Expression.t
  | Unsigned_lt of Expression.t * Expression.t
  (* Value fits in a (63-bit) OCaml int, i.e. is representable in Cconst_int *)
  | Fits_ocaml_int of Expression.t
  | And of assertion list

let rec smt_of_assertion : _ -> Sexp.t = function
  | Equal (a,b) ->
    List
      [ Atom "="
      ; Expression.smt_of_t a
      ; Expression.smt_of_t b
      ]
  | Signed_cmp (cmp, a, b) ->
    let a = Expression.smt_of_t a in
    let b = Expression.smt_of_t b in
    (match cmp with
     | EQ -> List [ Atom "="; a; b ]
     | NE -> List [ Atom "not"; List [ Atom "="; a; b ] ]
     | LT -> List [ Atom "bvslt"; a; b ]
     | GT -> List [ Atom "bvsgt"; a; b ]
     | LE -> List [ Atom "bvsle"; a; b ]
     | GE -> List [ Atom "bvsge"; a; b ])
  | Unsigned_lt (a, b) ->
    List [ Atom "bvult"; Expression.smt_of_t a; Expression.smt_of_t b ]
  | Fits_ocaml_int a ->
    List [ Atom "ocaml-highest-bits-same"; Expression.smt_of_t a ]
  | And [] -> Atom "true"
  | And [ a ] -> smt_of_assertion a
  | And l -> List (Atom "and" :: List.map l ~f:smt_of_assertion)

let smt_of_phrase : _ -> Sexp.t = function
  | Push i -> List [ Atom "push"; [%sexp_of:int] i ]
  | Pop i -> List [ Atom "pop"; [%sexp_of:int] i ]
  | Check_sat -> List [ Atom "check-sat" ]
  | Get_model -> List [ Atom "get-model" ]
  | Assert assertion ->
    List [ Atom "assert"; smt_of_assertion assertion ]
  | Assert_not assertion ->
    List [ Atom "assert-not"; smt_of_assertion assertion ]
  | Echo string ->
    List [ Atom "echo"; [%sexp_of: string] string ]
  | Declare_const { name; typ } ->
    List [ Atom "declare-const"; Atom name; Atom typ ]

