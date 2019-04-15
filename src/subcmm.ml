open! Core

module Const = struct
  type t =
    | Zero
    | One
    | Neg_one

  let smt_of_t = function
    | Zero    -> Sexp.Atom "#b00000"
    | One     -> Sexp.Atom "#b00001"
    | Neg_one -> Sexp.Atom "#b11111"

  let ocaml_of_t = function
    | Zero -> "0"
    | One  -> "1"
    | Neg_one -> "-1"
end

module Op = struct
  type t =
    | I of int_op
    | LI of int_log_op
    | CI of int_cmp_op
    | Tagi
  and int_op =
    | Lsl
    | Lsr
    | Asr
  and int_log_op =
    | Or
    | And
    | Xor
  and int_cmp_op =
    | EQ
    | NE
    | LT
    | GT
    | LE
    | GE

  let smt_of_int_op = function
    | Lsl -> Sexp.Atom "ocaml-lsl"
    | Lsr -> Sexp.Atom "ocaml-lsr"
    | Asr -> Sexp.Atom "ocaml-asr"

  let cmm_of_int_op = function
    | Lsl -> "Clsl"
    | Lsr -> "Clsr" 
    | Asr -> "Casr"

  let smt_of_int_log_op = function
    | Or -> Sexp.Atom "OR"
    | And -> Sexp.Atom "AND"
    | Xor -> Sexp.Atom "XOR"

  let cmm_of_int_log_op = function
    | Or -> "Cor"
    | And -> "Cand"
    | Xor -> "Cxor"

  let smt_of_int_cmp_op = function
    | EQ -> Sexp.Atom "EQ"
    | NE -> Sexp.Atom "NE"
    | LT -> Sexp.Atom "LT"
    | GT -> Sexp.Atom "GT"
    | LE -> Sexp.Atom "LE"
    | GE -> Sexp.Atom "GE"

  let cmm_of_int_cmp_op = function
    | EQ -> "Ccmpi Ceq"
    | NE -> "Ccmpi Cne"
    | LT -> "Ccmpi Clt"
    | GT -> "Ccmpi Cgt"
    | LE -> "Ccmpi Cle"
    | GE -> "Ccmpi Cge"

  let smt_of_t = function
    | Tagi -> [ Sexp.Atom "ocaml-tagi" ]
    | I io  -> [ smt_of_int_op io ]
    | LI li -> [ Sexp.Atom "ocaml-logi"; smt_of_int_log_op li ]
    | CI ci -> [ Sexp.Atom "ocaml-cmp"; smt_of_int_cmp_op ci ]
end

module Var = struct
  module T = struct
    type t =
      { name : string
      ; typ  : [`Int]
      } [@@deriving compare, sexp]
  end
  include T
  include Comparator.Make(T)

  let ocaml_of_t { name ; _ } =
    name

  let smt_of_t { name ; _ } =
    Sexp.Atom name

  let declare_of_t { name; typ } =
    let typ = 
      match typ with
      | `Int -> "ocaml-int"
    in
    Sexp.List
      [ Sexp.Atom "declare-const"
      ; Sexp.Atom name
      ; Sexp.Atom typ
      ]

  let create name =
    { name 
    ; typ = `Int
    }
end

module Var_action = struct
  type t =
    | Plain
    | Tag
    | Untag
end

module Expr = struct
  type t =
    | Const of Const.t
    | Var of Var.t
    | Op of 
        { op : Op.t
        ; args : t list
        }

  let rec cmm_pat_of_t = function
    | Const c ->
      sprintf "(Cconst_int (%s,_))" (Const.ocaml_of_t c)
    | Var v ->
      Var.ocaml_of_t v
    | Op { op; args } ->
      let args = 
        List.map args ~f:cmm_pat_of_t
        |> String.concat ~sep:"; "
      in
      match op with
      | Tagi ->
        sprintf "(Cop(Caddi,[Cop(Clsl, [%s; Cconst_int(1,_)],_); Cconst_int(1,_)],_))"
          args
      | I intop ->
        sprintf "(Cop(%s,[%s],_))" (Op.cmm_of_int_op intop) args
      | LI logop ->
        sprintf "(Cop(%s,[%s],_))" (Op.cmm_of_int_log_op logop) args
      | CI cmpop ->
        sprintf "(Cop(%s,[%s],_))" (Op.cmm_of_int_cmp_op cmpop) args


  let rec smt_of_t = function
    | Const c ->
      Const.smt_of_t c
    | Var v -> 
      Var.smt_of_t v
    | Op { op; args } ->
      let op = Op.smt_of_t op in
      Sexp.List
        (op @ (List.map args ~f:smt_of_t))

  let vars t =
    let set = Set.empty (module Var) in
    let rec aux set = function
      | Const _ -> set
      | Var v -> Set.add set v
      | Op { op = _; args } ->
        List.fold ~init:set args
          ~f:(fun set arg -> aux set arg)
    in
    aux set t
end

module Constraint = struct
  type t = unit
end

type t = 
  { input : Expr.t
  ; output : Expr.t
  ; constraints : Constraint.t list
  }

let assert_not a b =
  Sexp.List
    [ Sexp.Atom "assert-not"
    ; Sexp.List
        [ Sexp.Atom "="
        ; a
        ; b
        ]
    ]

let push i = [%message "push" ~_:(i : int)]
let pop i = [%message "pop" ~_:(i : int)]

let push_check_pop v =
  List.concat
    [ [ push 1
      ]
    ; v
    ; [ Sexp.List [ Sexp.Atom "check-sat" ]
      ; pop 1
      ]
    ]

let smt_of_t
    { input
    ; output
    ; constraints = _
    }
  =
  let constraints =
    let all_vars =
      Set.union (Expr.vars input) (Expr.vars output)
    in
    Set.to_list all_vars |> List.map ~f:Var.declare_of_t
  in
  let assertion =
    assert_not
      (Expr.smt_of_t input)
      (Expr.smt_of_t output)
  in
  List.concat 
    [ constraints
    ; [ assertion ]
    ]
  |> push_check_pop

