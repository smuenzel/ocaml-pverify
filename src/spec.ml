open! Core

module Const = struct
  type t =
    | Zero
    | One
    | Neg_one
  [@@deriving sexp]
end

module Unary_op = struct
  module Kind = struct
    type t =
      | Tagi
    [@@deriving sexp]
  end

  type 'param t = 
    { kind : Kind.t
    ; p1  : 'param
    } [@@deriving sexp]
end

module Binary_op = struct
  module Kind = struct
    module Shift = struct
      type t =
        | Lsl
        | Lsr
        | Asr
      [@@deriving sexp]
    end
    module Logical = struct
      type t =
        | Or
        | And
        | Xor
      [@@deriving sexp]
    end
    module Compare = struct
      type t =
        | EQ
        | NE
        | LT
        | GT
        | LE
        | GE
      [@@deriving sexp]
    end

    type t =
      | Shift of Shift.t
      | Logical of Logical.t
      | Compare of Compare.t
    [@@deriving sexp]
  end

  type 'param t = 
    { kind : Kind.t
    ; p1  : 'param
    ; p2  : 'param
    } [@@deriving sexp]

end

module Debug = struct
  module Source = struct
    module Name = String_id.Make(struct let module_name = "Spec.Debug.Source.Name" end)( )

    type t =
      | Anon
      | Set of Name.t
    [@@deriving sexp]
  end
  module Destination = struct
    type t =
      | Inherit
      | From_source of Source.Name.t
    [@@deriving sexp]
  end

end

module Var = struct
  module Name = String_id.Make(struct let module_name = "Spec.Var.Name" end)( )
  type t =
    { name : Name.t
    } [@@deriving sexp]
end

module Phrase = struct
  type 'debug kind =
    | Var of Var.t
    | Const of Const.t
    | Op1 of 'debug t Unary_op.t
    | Op2 of 'debug t Binary_op.t
  and 'debug t =
    { kind : 'debug kind
    ; debug : 'debug
    }
  [@@deriving sexp]

end

module Make = struct
  let const v debug =
    { Phrase.
      kind = Phrase.Const v
    ; debug
    }

  let var name =
    { Phrase.
      kind =
        Phrase.Var
          { Var.
            name = Var.Name.of_string name
          }
    ; debug = Debug.Source.Anon 
    }
  ,{ Phrase.
     kind =
       Phrase.Var
         { Var.
           name = Var.Name.of_string name
         }
   ; debug = Debug.Destination.Inherit
   }

  let op2 kind p1 p2 debug =
    { Phrase.
      kind =
        Phrase.Op2
          { Binary_op.
            kind
          ; p1
          ; p2
          }
    ; debug
    }

  let op1 kind p1 debug =
    { Phrase.
      kind =
        Phrase.Op1
          { Unary_op.
            kind
          ; p1
          }
    ; debug
    }
end
