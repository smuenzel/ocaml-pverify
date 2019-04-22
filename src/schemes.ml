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
