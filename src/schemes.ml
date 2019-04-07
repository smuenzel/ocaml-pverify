open! Core
open! Subcmm

let example =
  let left = Var.create "left" in
  let right = Var.create "right" in
  let (input : Expr.t) =
    Op
      { op = LI Or
      ; args =
          [ Op
              { op = LI Xor
              ; args =
                  [ Op { op = Tagi; args = [ Var left ] }
                  ; Op { op = Tagi; args = [ Var right ] }
                  ]
              }
          ; Const One
          ]
      }
  in
  let (output : Expr.t) =
    Op { op = Tagi
       ; args =
           [ Op { op = LI Xor; args = [ Var left; Var right ] } ]
       }
  in
  { input
  ; output
  ; constraints = []
  }
  
