open! Core

module Var = struct
  module Name =
    (val String_id.make
        ~module_name:"Spec.Var.Name"
        ~include_default_validation:true
        ~caller_identity:String_id.legacy_identity
        ()
    )

  type _ kind =
    | Int : [`Int] kind (* a runtime Cmm value *)
    | Cmp : [`Cmp] kind (* an integer comparison *)
    | Fcmp : [`Fcmp] kind (* a float comparison *)
    | Float : [`Float] kind (* a runtime unboxed float *)
    | Const : [`Const] kind (* a compile-time OCaml [int] on the (64-bit) host *)
    | Natconst : [`Natconst] kind (* a compile-time [nativeint] *)
  [@@deriving sexp_of]

  type 'kind t =
    { name : Name.t
    ; kind : 'kind kind
    } [@@deriving sexp_of]

  type packed =
    | T : _ t -> packed

  let singleton v = Name.Map.singleton v.name (T v)

  let merge a b =
    Map.merge a b
      ~f:(fun ~key -> function
          | `Left left -> Some left
          | `Right right -> Some right
          | `Both (left, right) ->
            if Poly.(left <> right)
            then raise_s [%message "vars of different types" (key : Name.t)]
            else Some left
        )

  let merge_all l = List.fold l ~init:Name.Map.empty ~f:merge
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
    | Three
    | Neg_one
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

module Float_compare = struct
  (* Mirrors [Lambda.float_comparison]: the N variants are negations, which
     differ from the swapped comparison in the presence of NaN. *)
  type t =
    | FEQ
    | FNEQ
    | FLT
    | FNLT
    | FGT
    | FNGT
    | FLE
    | FNLE
    | FGE
    | FNGE
  [@@deriving sexp]
end

(* A comparison operator: a constant, a variable, or computed at compile time
   with [negate_*_comparison] / [swap_*_comparison]. *)
module Icmp = struct
  type t =
    | K of Compare.t
    | V of [`Cmp] Var.t
    | Negate of t
    | Swap of t
  [@@deriving sexp_of]

  let rec all_vars = function
    | K _ -> Var.Name.Map.empty
    | V v -> Var.singleton v
    | Negate t | Swap t -> all_vars t
end

module Fcmp = struct
  type t =
    | K of Float_compare.t
    | V of [`Fcmp] Var.t
    | Negate of t
    | Swap of t
  [@@deriving sexp_of]

  let rec all_vars = function
    | K _ -> Var.Name.Map.empty
    | V v -> Var.singleton v
    | Negate t | Swap t -> all_vars t
end

(* Compile-time computations, as performed by the compiler on the host.
   [Int] operations wrap at 63 bits like OCaml's [int]; [Nativeint] ones at
   64 bits. *)
module Host = struct
  type width =
    | Int
    | Nativeint
  [@@deriving sexp_of]

  module Unop = struct
    type t =
      | Neg
      | Lnot
      | Log2 (* [Misc.log2] *)
    [@@deriving sexp_of]
  end

  module Binop = struct
    type t =
      | Add
      | Sub
      | Mul
      | Div
      | Mod
      | Land
      | Lor
      | Lxor
      | Lsl
      | Lsr
      | Asr
    [@@deriving sexp_of]
  end

  type expr =
    | Var of [`Const] Var.t
    | Natvar of [`Natconst] Var.t
    | Lit of int
    | Natlit of Int64.t
    | Unop of Unop.t * width * expr
    | Binop of Binop.t * width * expr * expr
    | Of_int of expr (* [Nativeint.of_int] *)
    | To_int of expr (* [Nativeint.to_int] *)
    | Compare of width * expr * expr (* [Int.compare] / [Nativeint.compare] *)
    | Ite of cond * expr * expr
  and cond =
    | Bool of bool
    | And of cond list
    | Or of cond list
    | Not of cond
    | Cmp of Compare.t * expr * expr (* signed *)
    | Ucmp of Compare.t * expr * expr (* unsigned *)
    (* [(m, p) = divimm_parameters d], characterised by the inequality
       documented in [Cmm_helpers]:
       2^(wordsize + p) < m * d <= 2^(wordsize + p) + 2^(p + 1),
       with [m] read as an unsigned word. *)
    | Divimm of { d : expr; m : expr; p : expr }
  [@@deriving sexp_of]

  let rec all_vars_expr = function
    | Var v -> Var.singleton v
    | Natvar v -> Var.singleton v
    | Lit _ | Natlit _ -> Var.Name.Map.empty
    | Unop (_, _, a) | Of_int a | To_int a -> all_vars_expr a
    | Binop (_, _, a, b) | Compare (_, a, b) ->
      Var.merge (all_vars_expr a) (all_vars_expr b)
    | Ite (c, a, b) ->
      Var.merge_all [ all_vars_cond c; all_vars_expr a; all_vars_expr b ]
  and all_vars_cond = function
    | Bool _ -> Var.Name.Map.empty
    | And l | Or l -> Var.merge_all (List.map l ~f:all_vars_cond)
    | Not c -> all_vars_cond c
    | Cmp (_, a, b) | Ucmp (_, a, b) ->
      Var.merge (all_vars_expr a) (all_vars_expr b)
    | Divimm { d; m; p } ->
      Var.merge_all [ all_vars_expr d; all_vars_expr m; all_vars_expr p ]

  (* Replaces host variables by literals *)
  let rec subst_expr ~(f : string -> Int64.t option) e =
    let r = subst_expr ~f in
    match e with
    | Var v ->
      (match f (Var.Name.to_string v.name) with Some n -> Lit (Int64.to_int_exn n) | None -> e)
    | Natvar v ->
      (match f (Var.Name.to_string v.name) with Some n -> Natlit n | None -> e)
    | Lit _ | Natlit _ -> e
    | Unop (op, w, a) -> Unop (op, w, r a)
    | Binop (op, w, a, b) -> Binop (op, w, r a, r b)
    | Of_int a -> Of_int (r a)
    | To_int a -> To_int (r a)
    | Compare (w, a, b) -> Compare (w, r a, r b)
    | Ite (c, a, b) -> Ite (subst_cond ~f c, r a, r b)
  and subst_cond ~f c =
    let r = subst_cond ~f and e = subst_expr ~f in
    match c with
    | Bool _ -> c
    | And l -> And (List.map l ~f:r)
    | Or l -> Or (List.map l ~f:r)
    | Not c -> Not (r c)
    | Cmp (op, a, b) -> Cmp (op, e a, e b)
    | Ucmp (op, a, b) -> Ucmp (op, e a, e b)
    | Divimm { d; m; p } -> Divimm { d = e d; m = e m; p = e p }

  (* Shorthands for host [int] arithmetic *)
  module O = struct
    let ( + ) a b = Binop (Add, Int, a, b)
    let ( - ) a b = Binop (Sub, Int, a, b)
    let ( * ) a b = Binop (Mul, Int, a, b)
    let ( / ) a b = Binop (Div, Int, a, b)
    let ( mod ) a b = Binop (Mod, Int, a, b)
    let ( land ) a b = Binop (Land, Int, a, b)
    let ( lor ) a b = Binop (Lor, Int, a, b)
    let ( lxor ) a b = Binop (Lxor, Int, a, b)
    let ( lsl ) a b = Binop (Lsl, Int, a, b)
    let ( lsr ) a b = Binop (Lsr, Int, a, b)
    let ( asr ) a b = Binop (Asr, Int, a, b)
    let lnot a = Unop (Lnot, Int, a)
    let neg a = Unop (Neg, Int, a)
    let log2 a = Unop (Log2, Int, a)
    let i n = Lit n
    let min_int = Lit Int.min_value
    let max_int = Lit Int.max_value
    let ( = ) a b = Cmp (EQ, a, b)
    let ( <> ) a b = Cmp (NE, a, b)
    let ( < ) a b = Cmp (LT, a, b)
    let ( > ) a b = Cmp (GT, a, b)
    let ( <= ) a b = Cmp (LE, a, b)
    let ( >= ) a b = Cmp (GE, a, b)
    let ( && ) a b = And [ a; b ]
    let ( || ) a b = Or [ a; b ]
    let not a = Not a
    let true_ = Bool true
  end

  (* The predicates of [Misc], transcribed from utils/misc.ml *)
  module Misc = struct
    open O

    let no_overflow_add a b = (a lxor b) lor (a lxor (lnot (a + b))) < i 0

    let no_overflow_sub a b = (a lxor (lnot b)) lor (b lxor (a - b)) < i 0

    let no_overflow_mul a b =
      not ((a = min_int && b < i 0) || (b <> i 0 && (a * b) / b <> a))

    let no_overflow_lsl a k =
      i 0 <= k && k < i 63 (* Sys.word_size - 1 *) && (min_int asr k) <= a && a <= (max_int asr k)

    (* [Cmm_helpers.is_power2] *)
    let is_power2 n = n = (i 1 lsl log2 n)
  end
end

module Unary_op = struct
  module Kind = struct
    type t =
      | Tagi
      | Untagi
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
    module Arith = struct
      type t =
        | Add
        | Sub
        | Mul
        | Mulhi
        | Div
        | Mod
      [@@deriving sexp]
    end
    module Compare = Compare

    type t =
      | Shift of Shift.t
      | Logical of Logical.t
      | Arith of Arith.t
      | Compare of Icmp.t (* Ccmpi *)
      | Compare_unsigned of Icmp.t (* Ccmpa *)
    [@@deriving sexp_of]

    let all_vars = function
      | Shift _
      | Logical _
      | Arith _ ->
        Var.Name.Map.empty
      | Compare c | Compare_unsigned c -> Icmp.all_vars c
  end

  type 'param t =
    { kind : Kind.t
    ; p1  : 'param
    ; p2  : 'param
    } [@@deriving sexp_of]

end

module Chunk = struct
  type t =
    | Byte_unsigned
    | Byte_signed
    | Sixteen_unsigned
    | Sixteen_signed
    | Thirtytwo_unsigned
    | Thirtytwo_signed
    | Word
  [@@deriving sexp]
end

module Endian = struct
  type t =
    | Little
    | Big
  [@@deriving sexp]
end

module Debug = struct
  module Source = struct
    module Name =
      (val String_id.make
          ~module_name:"Spec.Debug.Source.Name"
          ~include_default_validation:true
          ~caller_identity:String_id.legacy_identity
          ()
      )

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
    | Const_int of Host.expr (* Cconst_int *)
    | Const_natint of Host.expr (* Cconst_natint *)
    | Op1 of 'debug t Unary_op.t
    | Op2 of 'debug t Binary_op.t
    | Fcompare of { cmp : Fcmp.t; f1 : [`Float] Var.t; f2 : [`Float] Var.t }
    | Seq of 'debug t * 'debug t (* Csequence *)
    | Ite of 'debug t * 'debug t * 'debug t (* Cifthenelse *)
    | Load of { chunk : Chunk.t; endian : Endian.t; addr : 'debug t }
    (* Some value that agrees with [arg] on the bits set in [mask] (a
       [nativeint]). Models helpers such as [ignore_low_bit_int] whose
       specification is a relation rather than a function. *)
    | Masked of { mask : Host.expr; arg : 'debug t }
  and 'debug t =
    { kind : 'debug kind
    ; debug : 'debug
    }
  [@@deriving sexp_of]

  let rec all_vars { kind; debug = _ } : Var.packed Var.Name.Map.t =
    match kind with
    | Var var -> Var.singleton var
    | Const _ ->
      Var.Name.Map.empty
    | Const_int e | Const_natint e -> Host.all_vars_expr e
    | Op1 { p1; _ } -> all_vars p1
    | Op2 { p1; p2; kind } ->
      Var.merge (all_vars p1) (all_vars p2)
      |> Var.merge (Binary_op.Kind.all_vars kind)
    | Fcompare { cmp; f1; f2 } ->
      Var.merge_all [ Fcmp.all_vars cmp; Var.singleton f1; Var.singleton f2 ]
    | Seq (a, b) -> Var.merge (all_vars a) (all_vars b)
    | Ite (a, b, c) -> Var.merge_all [ all_vars a; all_vars b; all_vars c ]
    | Load { chunk = _; endian = _; addr } -> all_vars addr
    | Masked { mask; arg } -> Var.merge (Host.all_vars_expr mask) (all_vars arg)

  let rec subst ~f ({ kind; debug } as p) =
    let r = subst ~f in
    let kind =
      match kind with
      | Var _ | Const _ | Fcompare _ -> kind
      | Const_int e -> Const_int (Host.subst_expr ~f e)
      | Const_natint e -> Const_natint (Host.subst_expr ~f e)
      | Op1 { kind; p1 } -> Op1 { kind; p1 = r p1 }
      | Op2 { kind; p1; p2 } -> Op2 { kind; p1 = r p1; p2 = r p2 }
      | Seq (a, b) -> Seq (r a, r b)
      | Ite (a, b, c) -> Ite (r a, r b, r c)
      | Load { chunk; endian; addr } -> Load { chunk; endian; addr = r addr }
      | Masked { mask; arg } -> Masked { mask = Host.subst_expr ~f mask; arg = r arg }
    in
    ignore p;
    { kind; debug }

  let rec has_load { kind; debug = _ } =
    match kind with
    | Load _ -> true
    | Var _ | Const _ | Const_int _ | Const_natint _ | Fcompare _ -> false
    | Op1 { p1; _ } -> has_load p1
    | Op2 { p1; p2; _ } | Seq (p1, p2) -> has_load p1 || has_load p2
    | Ite (a, b, c) -> has_load a || has_load b || has_load c
    | Masked { arg; _ } -> has_load arg
end

(* Facts about the runtime values of the input that the rewrite may rely on,
   e.g. that an argument is a tagged integer. *)
module Pre = struct
  type 'phrase t =
    | Tagged of 'phrase (* low bit set *)
    | Is_bool of 'phrase (* 0 or 1 *)
    | Tagged_bool of 'phrase (* 1 or 3 *)
    | Cmp of Compare.t * 'phrase * 'phrase (* signed *)
  [@@deriving sexp_of]

  let all_vars = function
    | Tagged p | Is_bool p | Tagged_bool p -> Phrase.all_vars p
    | Cmp (_, a, b) -> Var.merge (Phrase.all_vars a) (Phrase.all_vars b)
end

(* How the output must relate to the input *)
module Relation = struct
  type t =
    | Equal
    (* Equal on the bits set in the mask (a [nativeint]) *)
    | Agree_on of Host.expr
    (* The output, read as an untagged truth value (zero or not), says the
       same as the input read as a tagged boolean (1 is false). *)
    | Truthiness
  [@@deriving sexp_of]
end

module Rule = struct
  type t =
    { input : Debug.Source.t Phrase.t
    ; output : Debug.Destination.t Phrase.t
    ; guard : Host.cond
    ; pre : Debug.Source.t Phrase.t Pre.t list
    ; relation : Relation.t
    } [@@deriving sexp_of]

  let all_vars { input; output; guard; pre; relation } =
    Var.merge_all
      ([ Phrase.all_vars input
       ; Phrase.all_vars output
       ; Host.all_vars_cond guard
       ; (match relation with
           | Equal | Truthiness -> Var.Name.Map.empty
           | Agree_on mask -> Host.all_vars_expr mask)
       ]
       @ List.map pre ~f:Pre.all_vars)

  let has_load { input; output; _ } =
    Phrase.has_load input || Phrase.has_load output

  (* Fixes host constants to the given values *)
  let instantiate { input; output; guard; pre; relation } ~values =
    let f name = List.Assoc.find values name ~equal:String.equal in
    let subst_pre : _ Pre.t -> _ Pre.t = function
      | Tagged p -> Tagged (Phrase.subst ~f p)
      | Is_bool p -> Is_bool (Phrase.subst ~f p)
      | Tagged_bool p -> Tagged_bool (Phrase.subst ~f p)
      | Cmp (c, a, b) -> Cmp (c, Phrase.subst ~f a, Phrase.subst ~f b)
    in
    { input = Phrase.subst ~f input
    ; output = Phrase.subst ~f output
    ; guard = Host.subst_cond ~f guard
    ; pre = List.map pre ~f:subst_pre
    ; relation =
        (match relation with
         | Equal | Truthiness -> relation
         | Agree_on mask -> Agree_on (Host.subst_expr ~f mask))
    }
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

  let var_const name =
    { Var.
      name = Var.Name.of_string name
    ; kind = Const
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
