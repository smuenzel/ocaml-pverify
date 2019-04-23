open! Core
open! Subcmm

let example =
  let left = Var.create "left" in
  let right = Var.create "right" in
  let (input : Expr.t) =
    Op
      { op = `LI Or
      ; args =
          [ Op
              { op = `LI Xor
              ; args =
                  [ Op { op = `Tagi; args = [ Var left ] }
                  ; Op { op = `Tagi; args = [ Var right ] }
                  ]
              }
          ; Const One
          ]
      }
  in
  let (output : Expr.t) =
    Op { op = `Tagi
       ; args =
           [ Op { op = `LI Xor; args = [ Var left; Var right ] } ]
       }
  in
  { input
  ; output
  ; constraints = []
  }
  
let op2 op arg1 arg2 : Expr.t =
  Op
    { op
    ; args = [ arg1; arg2 ]
    }

let op1 op arg1 : Expr.t =
  Op
    { op
    ; args = [ arg1 ]
    }

let one : Expr.t = Const One
  

let example2 =
  let left = Var.create "left" in
  let right = Var.create "right" in
  let (input : Expr.t) =
    op2 (`LI Or)
      (op2 (`LI Xor)
         (op1 `Tagi (Var left))
         (op1 `Tagi (Var right))
      )
      one
  in
  let (output : Expr.t) =
    op1 `Tagi (op2 (`LI Xor) (Var left) (Var right))
  in
  { input
  ; output
  ; constraints = []
  }

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
