open! Core

let example_spec =
  let open Spec.Make in
  let left_s, left_d = var "left" in
  let right_s, right_d = var "right" in
  let dbg_name = Spec.Debug.Source.Name.of_string "dbg_xor" in
  let source =
    let dbg = Spec.Debug.Source.Anon in
    let debug_s = Spec.Debug.Source.Set dbg_name in
    op2 (Logical Or)
      (op2 (Logical Xor)
         (op1 Tagi left_s dbg)
         (op1 Tagi right_s dbg)
         debug_s
      )
      (const One dbg)
      dbg
  in
  let destination =
    let dbg = Spec.Debug.Destination.From_source dbg_name in
    op1 Tagi (op2 (Logical Xor) left_d right_d dbg) dbg
  in
  source
, destination
