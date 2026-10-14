open! Core
open Smt.Term

let var_name (v : _ Spec.Var.t) = Spec.Var.Name.to_string v.name

let smt_of_const (s_c : Spec.Const.t) =
  match s_c with
  | One -> int 1
  | Three -> int 3
  | Zero -> int 0
  | Neg_one -> int (-1)

let smt_of_compare (c : Spec.Compare.t) = Sexp.Atom (Spec.Compare.sexp_of_t c |> Sexp.to_string)

let rec smt_of_icmp : Spec.Icmp.t -> Sexp.t = function
  | K c -> smt_of_compare c
  | V v -> atom (var_name v)
  | Negate c -> app "cmp-negate" [ smt_of_icmp c ]
  | Swap c -> app "cmp-swap" [ smt_of_icmp c ]

let rec smt_of_fcmp : Spec.Fcmp.t -> Sexp.t = function
  | K c -> Sexp.Atom (Spec.Float_compare.sexp_of_t c |> Sexp.to_string)
  | V v -> atom (var_name v)
  | Negate c -> app "fcmp-negate" [ smt_of_fcmp c ]
  | Swap c -> app "fcmp-swap" [ smt_of_fcmp c ]

let signed_cmp (c : Spec.Compare.t) a b =
  match c with
  | EQ -> eq a b
  | NE -> ne a b
  | LT -> app "bvslt" [ a; b ]
  | GT -> app "bvsgt" [ a; b ]
  | LE -> app "bvsle" [ a; b ]
  | GE -> app "bvsge" [ a; b ]

let unsigned_cmp (c : Spec.Compare.t) a b =
  match c with
  | EQ -> eq a b
  | NE -> ne a b
  | LT -> app "bvult" [ a; b ]
  | GT -> app "bvugt" [ a; b ]
  | LE -> app "bvule" [ a; b ]
  | GE -> app "bvuge" [ a; b ]

(* Host computations. Returns the value, and conditions under which the
   computation is well defined on the host (no division by zero, shift
   amounts within range). *)
module Host = struct
  open Spec.Host

  let wrap (w : width) x =
    match w with
    | Int -> app "host-wrap" [ x ]
    | Nativeint -> x

  let rec expr (e : expr) : Sexp.t * Sexp.t list =
    match e with
    | Var v -> atom (var_name v), []
    | Natvar v -> atom (var_name v), []
    | Lit i -> int i, []
    | Natlit i -> int64 i, []
    | Of_int a -> expr a
    | To_int a ->
      let a, da = expr a in
      wrap Int a, da
    | Unop (op, w, a) ->
      let a, da = expr a in
      let v =
        match op with
        | Neg -> wrap w (app "bvneg" [ a ])
        | Lnot -> app "bvnot" [ a ]
        | Log2 -> app "host-log2" [ a ]
      in
      v, da
    | Binop (op, w, a, b) ->
      let a, da = expr a in
      let b, db = expr b in
      let shift_ok =
        (* OCaml leaves shifts by [n < 0] or [n > Sys.int_size] unspecified;
           for nativeint, by [n >= 64]. *)
        let max = match w with Int -> 63 | Nativeint -> 63 in
        [ app "bvsge" [ b; zero ]; app "bvsle" [ b; int max ] ]
      in
      let v, d =
        match op with
        | Add -> wrap w (app "bvadd" [ a; b ]), []
        | Sub -> wrap w (app "bvsub" [ a; b ]), []
        | Mul -> wrap w (app "bvmul" [ a; b ]), []
        | Div -> wrap w (app "bvsdiv" [ a; b ]), [ ne b zero ]
        | Mod -> app "bvsrem" [ a; b ], [ ne b zero ]
        | Land -> app "bvand" [ a; b ], []
        | Lor -> app "bvor" [ a; b ], []
        | Lxor -> app "bvxor" [ a; b ], []
        | Lsl -> wrap w (app "bvshl" [ a; b ]), shift_ok
        | Lsr ->
          (match w with
           | Int -> app "host-lsr" [ a; b ]
           | Nativeint -> app "bvlshr" [ a; b ]),
          shift_ok
        | Asr -> app "bvashr" [ a; b ], shift_ok
      in
      v, d @ da @ db
    | Compare (w, a, b) ->
      let a, da = expr a in
      let b, db = expr b in
      ignore w;
      app "host-compare" [ a; b ], da @ db
    | Ite (c, a, b) ->
      let a, da = expr a in
      let b, db = expr b in
      let c' = cond c in
      ite c' a b, [ implies c' (and_ da); implies (not_ c') (and_ db) ]

  and cond (c : cond) : Sexp.t =
    match c with
    | Bool true -> true_
    | Bool false -> false_
    | And l -> and_ (List.map l ~f:cond)
    | Or l -> or_ (List.map l ~f:cond)
    | Not c -> not_ (cond c)
    | Cmp (c, a, b) -> signed_cmp c (fst (expr a)) (fst (expr b))
    | Ucmp (c, a, b) -> unsigned_cmp c (fst (expr a)) (fst (expr b))
    | Divimm { d; m; p } ->
      app "divimm-ok" [ fst (expr d); fst (expr m); fst (expr p) ]
end

(* Fresh variables and constraints introduced while translating a phrase
   ([Masked]). *)
type ctx =
  { mutable counter : int
  ; mutable fresh : string list
  ; mutable assumptions : Sexp.t list
  }

let load_fun (chunk : Spec.Chunk.t) (endian : Spec.Endian.t) =
  let e = match endian with Little -> "le" | Big -> "be" in
  let c =
    match chunk with
    | Byte_unsigned -> "u8"
    | Byte_signed -> "s8"
    | Sixteen_unsigned -> "u16"
    | Sixteen_signed -> "s16"
    | Thirtytwo_unsigned -> "u32"
    | Thirtytwo_signed -> "s32"
    | Word -> "u64"
  in
  sprintf "load-%s-%s" c e

(* Translates a phrase to its value and the conditions under which it is
   defined in Cmm:
   - shift amounts must lie in [0, size_int * 8): Cmm leaves other amounts
     unspecified (amd64 masks the amount, arm64 rejects immediates);
   - division and modulus trap on a zero divisor and on [min_int / -1];
   - compile-time constants must be computable on the host. *)
let rec phrase ctx (p : _ Spec.Phrase.t) : Sexp.t * Sexp.t list =
  match p.kind with
  | Var v -> atom (var_name v), []
  | Const c -> smt_of_const c, []
  | Const_int e | Const_natint e -> Host.expr e
  | Op1 { kind; p1 } ->
    let a, da = phrase ctx p1 in
    let f = match kind with Tagi -> "ocaml-tagi" | Untagi -> "ocaml-untagi" in
    app f [ a ], da
  | Op2 { kind; p1; p2 } ->
    let a, da = phrase ctx p1 in
    let b, db = phrase ctx p2 in
    let v, d =
      match kind with
      | Shift s ->
        let f =
          match s with Lsl -> "ocaml-lsl" | Lsr -> "ocaml-lsr" | Asr -> "ocaml-asr"
        in
        app f [ a; b ], [ app "bvult" [ b; int 64 ] ]
      | Logical l ->
        let f =
          match l with Or -> "ocaml-or" | And -> "ocaml-and" | Xor -> "ocaml-xor"
        in
        app f [ a; b ], []
      | Arith ar ->
        (match ar with
         | Add -> app "ocaml-addi" [ a; b ], []
         | Sub -> app "ocaml-subi" [ a; b ], []
         | Mul -> app "ocaml-muli" [ a; b ], []
         | Mulhi -> app "ocaml-mulhi" [ a; b ], []
         | Div | Mod ->
           let f = match ar with Div -> "ocaml-divi" | _ -> "ocaml-modi" in
           app f [ a; b ],
           [ ne b zero
           ; not_ (and_ [ eq a (atom "#x8000000000000000"); eq b (int (-1)) ]) ])
      | Compare c -> app "ocaml-cmp" [ smt_of_icmp c; a; b ], []
      | Compare_unsigned c -> app "ocaml-cmpa" [ smt_of_icmp c; a; b ], []
    in
    v, d @ da @ db
  | Fcompare { cmp; f1; f2 } ->
    app "ocaml-cmpf" [ smt_of_fcmp cmp; atom (var_name f1); atom (var_name f2) ], []
  | Seq (a, b) ->
    let _, da = phrase ctx a in
    let b, db = phrase ctx b in
    b, da @ db
  | Ite (c, a, b) ->
    let c, dc = phrase ctx c in
    let a, da = phrase ctx a in
    let b, db = phrase ctx b in
    let true_c = ne c zero in
    ite true_c a b,
    dc @ [ implies true_c (and_ da); implies (not_ true_c) (and_ db) ]
  | Load { chunk; endian; addr } ->
    let a, da = phrase ctx addr in
    app (load_fun chunk endian) [ atom "mem"; a ], da
  | Masked { mask; arg } ->
    let a, da = phrase ctx arg in
    let m, dm = Host.expr mask in
    ctx.counter <- ctx.counter + 1;
    let name = sprintf "masked_%i" ctx.counter in
    ctx.fresh <- name :: ctx.fresh;
    let r = atom name in
    ctx.assumptions <-
      eq (app "bvand" [ r; m ]) (app "bvand" [ a; m ]) :: ctx.assumptions;
    r, dm @ da

let smt_of_pre ctx (pre : _ Spec.Pre.t) =
  let v p = fst (phrase ctx p) in
  match pre with
  | Tagged p -> app "ocaml-has-tag" [ v p ]
  | Is_bool p -> or_ [ eq (v p) zero; eq (v p) one ]
  | Tagged_bool p -> or_ [ eq (v p) one; eq (v p) (int 3) ]
  | Cmp (c, a, b) -> signed_cmp c (v a) (v b)

let relation (r : Spec.Relation.t) ~input ~output =
  match r with
  | Equal -> eq input output
  | Agree_on mask ->
    let m, _ = Host.expr mask in
    eq (app "bvand" [ input; m ]) (app "bvand" [ output; m ])
  | Truthiness -> eq (ne output zero) (ne input one)

let sort_of_var (Spec.Var.T { kind; _ }) =
  match kind with
  | Int -> "ocaml-int"
  | Cmp -> "CMP"
  | Fcmp -> "FCMP"
  | Float -> "ocaml-float"
  | Const -> "ocaml-int"
  | Natconst -> "ocaml-int"

(* Refinement: whenever the guard and the preconditions hold and the input
   is defined, the output must be defined and related to the input. *)
let verify ?(get_values = false) ?(check_vacuity = false) name (rule : Spec.Rule.t)
  : Smt.phrase list =
  let open Smt in
  let vars = Spec.Rule.all_vars rule in
  let declare_vars =
    Map.fold_right vars ~init:[]
      ~f:(fun ~key:_ ~data acc ->
          let Spec.Var.T { name; _ } = data in
          Declare_const { name = Spec.Var.Name.to_string name
                        ; typ = sort_of_var data
                        }
          :: acc
        )
  in
  (* Host [int] constants fit in 63 bits *)
  let host_ints =
    Map.data vars
    |> List.filter_map ~f:(fun (Spec.Var.T v as packed) ->
        match v.kind with
        | Const -> Some (app "ocaml-highest-bits-same" [ atom (var_name v) ])
        | _ -> ignore packed; None)
  in
  let ctx = { counter = 0; fresh = []; assumptions = [] } in
  let input, input_defined = phrase ctx rule.input in
  let output, output_defined = phrase ctx rule.output in
  let pre = List.map rule.pre ~f:(smt_of_pre ctx) in
  let declare_fresh =
    List.rev_map ctx.fresh ~f:(fun name -> Declare_const { name; typ = "ocaml-int" })
  in
  let declare_mem =
    if Spec.Rule.has_load rule
    then [ Declare_const { name = "mem"; typ = "ocaml-mem" } ]
    else []
  in
  let get_values =
    if get_values
    then
      [ Get_value
          (Map.data vars
           |> List.filter_map ~f:(fun (Spec.Var.T v) ->
               match v.kind with
               | Int | Const | Natconst | Cmp | Fcmp | Float -> Some (var_name v)))
      ]
    else []
  in
  let assumptions =
    Assert
      (and_
         (Host.cond rule.guard
          :: (host_ints @ pre @ input_defined @ List.rev ctx.assumptions)))
  in
  (* A rule whose assumptions are unsatisfiable verifies vacuously *)
  let vacuity =
    if check_vacuity
    then [ Push 1; Echo "assumptions satisfiable?"; assumptions; Check_sat; Pop 1 ]
    else []
  in
  List.concat
    [ [ Push 1
      ; Echo ("verifying " ^ name)
      ]
    ; declare_vars
    ; declare_mem
    ; declare_fresh
    ; vacuity
    ; [ assumptions
      ; Assert
          (not_
             (and_
                (output_defined
                 @ [ relation rule.relation ~input ~output ])))
      ; Check_sat
      ]
    ; get_values
    ; [ Pop 1 ]
    ]
