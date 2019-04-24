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
      | Cmp of Compare.t

    let smt_op2 name p1 p2 =
      Sexp.List
        [ Sexp.Atom name
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
      | Cmp cmp -> 
        fun p1 p2 ->
        Sexp.List
          [ Sexp.Atom "ocaml-cmp"
          ; [%sexp_of: Compare.t] cmp
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

module Var = String_id.Make(struct let module_name = "Smt.Var" end)( )

module Const = struct
  type t =
    | Zero
    | One
    | Neg_one
end

module Expression = struct
  type t =
    | Const of Const.t
    | Var of Var.t
    | Ocaml_op2 of t Ocaml_op2.t

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
  | Assert_not of assertion
and assertion =
  | Equal of Expression.t * Expression.t

