open! Core

module Var = struct
  module Name = String_id.Make(struct let module_name = "Spec.Var.Name" end)( )
  type _ kind =
    | Int : [`Int] kind
    | Cmp : [`Cmp] kind
  [@@deriving sexp_of]

  type 'kind t =
    { name : Name.t
    ; kind : 'kind kind
    } [@@deriving sexp_of]

  type packed =
    | T : _ t -> packed

  let merge a b =
    Map.merge a b
      ~f:(fun ~key -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            if left <> right
            then raise_s [%message "vars of different types" (key : Name.t)]
            else Some left
        )
end

module Or_variable = struct
  type ('vartype, 'kind) t =
    | V of 'vartype Var.t
    | K of 'kind
  [@@deriving sexp_of]

  let map ~f = function
    | V v -> V v
    | K k -> K (f k)
end

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
      | Compare of ([`Cmp], Compare.t) Or_variable.t
    [@@deriving sexp_of]

    let all_vars = function
      | Shift _
      | Logical _
      | Compare (K _) ->
        Var.Name.Map.empty
      | Compare (V v) ->
        Var.Name.Map.singleton v.name (Var.T v)
  end

  type 'param t = 
    { kind : Kind.t
    ; p1  : 'param
    ; p2  : 'param
    } [@@deriving sexp_of]

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

module Phrase = struct
  type 'debug kind =
    | Var of [`Int] Var.t
    | Const of Const.t
    | Op1 of 'debug t Unary_op.t
    | Op2 of 'debug t Binary_op.t
  and 'debug t =
    { kind : 'debug kind
    ; debug : 'debug
    }
  [@@deriving sexp_of]

  let rec all_vars { kind; debug = _ } =
    match kind with
    | Var var ->
      Var.Name.Map.singleton var.name (Var.T var)
    | Const _ -> 
      Var.Name.Map.empty
    | Op1 { p1; _ } -> all_vars p1  
    | Op2 { p1; p2; kind } ->
      Var.merge (all_vars p1) (all_vars p2)
      |> Var.merge (Binary_op.Kind.all_vars kind)

end

module Rule = struct
  type t =
    { input : Debug.Source.t Phrase.t
    ; output : Debug.Destination.t Phrase.t
    } [@@deriving sexp_of]

  let all_vars { input; output } =
    Var.merge (Phrase.all_vars input) (Phrase.all_vars output)
end

module Make = struct
  let const v debug =
    { Phrase.
      kind = Phrase.Const v
    ; debug
    }

  let var_cmp name =
    { Var.
      name = Var.Name.of_string name
    ; kind = Cmp
    }

  let var name =
    { Phrase.
      kind =
        Phrase.Var
          { Var.
            name = Var.Name.of_string name
          ; kind = Int
          }
    ; debug = Debug.Source.Anon 
    }
  ,{ Phrase.
     kind =
       Phrase.Var
         { Var.
           name = Var.Name.of_string name
         ; kind = Int
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
