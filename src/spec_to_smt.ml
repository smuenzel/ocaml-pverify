open! Core

let smt_of_const (s_c : Spec.Const.t) : Smt.Expression.t =
  match s_c with
  | One -> Const One
  | Three -> Const Three
  | Zero -> Const Zero
  | Neg_one -> Const Neg_one

let smt_of_const_var (var : _ Spec.Var.t) ~offset : Smt.Expression.t =
  let v : Smt.Expression.t =
    Var (Smt.Var.of_string (Spec.Var.Name.to_string var.name))
  in
  if offset = 0
  then v
  else Ocaml_op2 { kind = Addi; p1 = v; p2 = Const (Int offset) }

let rec smt_of_phrase (phrase : _ Spec.Phrase.t) : Smt.Expression.t =
  let { Spec.Phrase.
        kind
      ; debug = _
      } = phrase 
  in
  match kind with
  | Var v -> Var (Smt.Var.of_string (Spec.Var.Name.to_string v.name))
  | Const c -> smt_of_const c
  | Const_var { var; offset } -> smt_of_const_var var ~offset
  | Op1 op1 -> smt_of_op1 op1
  | Op2 op2 -> smt_of_op2 op2

and smt_of_op2 op2 =
  let { Spec.Binary_op.
        kind
      ; p1
      ; p2
      } = op2
  in
  let (smt_op : Smt.Ocaml_op2.Kind.t) =
    match kind with
    | Shift Lsl -> Lsl
    | Shift Lsr -> Lsr
    | Shift Asr -> Asr
    | Logical Or -> Or
    | Logical And -> And
    | Logical Xor -> Xor
    | Compare cmp -> Cmp cmp
  in
  let p1 = smt_of_phrase p1 in
  let p2 = smt_of_phrase p2 in
  Smt.Expression.Ocaml_op2
    { kind = smt_op
    ; p1
    ; p2
    }

and smt_of_op1 op1 =
  let { Spec.Unary_op.
        kind
      ; p1
      } = op1
  in
  let p1 = smt_of_phrase p1 in
  match kind with
  | Tagi ->
    Ocaml_op2
      { kind = Addi
      ; p1 =
          Ocaml_op2
            { kind = Lsl
            ; p1
            ; p2 = Const One
            }
      ; p2 = Const One
      }
  | Untagi ->
    Ocaml_op2
      { kind = Asr
      ; p1
      ; p2 = Const One
      }

(* Conditions under which a phrase has a defined value in Cmm:
   - shift amounts must lie in [0, size_int * 8) (Cmm leaves other amounts
     unspecified; e.g. amd64 masks the amount, arm64 rejects it);
   - symbolic constants must be representable in a [Cconst_int]. *)
let rec defined_of_phrase (phrase : _ Spec.Phrase.t) : Smt.assertion list =
  match phrase.kind with
  | Var _
  | Const _ -> []
  | Const_var { var; offset } ->
    [ Fits_ocaml_int (smt_of_const_var var ~offset) ]
  | Op1 { kind = _; p1 } -> defined_of_phrase p1
  | Op2 { kind; p1; p2 } ->
    let shift =
      match kind with
      | Shift _ ->
        [ Smt.Unsigned_lt
            (smt_of_phrase p2, Const (Int Smt.Smt_int.size_in_bits)) ]
      | Logical _ | Compare _ -> []
    in
    shift @ defined_of_phrase p1 @ defined_of_phrase p2

let smt_of_guard_operand : Spec.Guard.Operand.t -> Smt.Expression.t = function
  | Const_var (var, offset) -> smt_of_const_var var ~offset
  | Word_bits offset -> Const (Int (Smt.Smt_int.size_in_bits + offset))
  | Literal i -> Const (Int i)

let smt_of_guard { Spec.Guard. cmp; lhs; rhs } : Smt.assertion =
  Signed_cmp (cmp, smt_of_guard_operand lhs, smt_of_guard_operand rhs)

(* Refinement: whenever the guard holds and the input is defined, the output
   must be defined and equal to the input. *)
let assume_rule { Spec.Rule. input; output = _; guard } : Smt.phrase =
  Assert (And (List.map guard ~f:smt_of_guard @ defined_of_phrase input))

let assert_not_rule { Spec.Rule. input; output; guard = _ } : Smt.phrase =
  Assert_not
    (And
       (defined_of_phrase output
        @ [ Equal (smt_of_phrase input, smt_of_phrase output) ]))

let verify name rule : Smt.phrase list =
  let open Smt in
  let vars = Spec.Rule.all_vars rule in
  let declare_vars =
    Map.fold vars ~init:[]
      ~f:(fun ~key:_ ~data acc ->
          let Spec.Var.T { name; kind } = data in
          let typ =
            match kind with
            | Int -> "ocaml-int"
            | Cmp -> "CMP"
            | Const -> "ocaml-int"
          in
          Declare_const { name = Spec.Var.Name.to_string name
                        ; typ
                        }
          :: acc
        )
  in
  List.concat
    [ [ Push 1
      ; Echo ("verifying " ^ name)
      ]
    ; declare_vars
    ; [ assume_rule rule
      ; assert_not_rule rule
      ; Check_sat
      ; Pop 1
      ]
    ]

