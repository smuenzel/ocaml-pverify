open! Core

let smt_of_const (s_c : Spec.Const.t) : Smt.Expression.t =
  match s_c with
  | One -> Const One
  | Zero -> Const Zero
  | Neg_one -> Const Neg_one

let rec smt_of_phrase (phrase : _ Spec.Phrase.t) : Smt.Expression.t =
  let { Spec.Phrase.
        kind
      ; debug = _
      } = phrase 
  in
  match kind with
  | Var v -> Var (Smt.Var.of_string (Spec.Var.Name.to_string v.name))
  | Const c -> smt_of_const c
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
  let Tagi = kind in
  let p1 = smt_of_phrase p1 in
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

let assert_not_rule { Spec.Rule. input; output } : Smt.phrase =
  Assert_not (Equal (smt_of_phrase input, smt_of_phrase output))

