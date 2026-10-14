open! Core
open Smt.Term

(* A second encoding of rules, in integer arithmetic instead of bit-vectors.

   Every 64-bit word is an integer in [-2^63, 2^63) (host [int]s in
   [-2^62, 2^62)); wrap-around is explicit: [wrap e] is a fresh [v] with
   [v = e - k * 2^64] for an integer [k], and [v] in range. Multiplication of
   two variables stays a single nonlinear term, which the solver handles far
   better than a bit-blasted multiplier.

   Only arithmetic, shifts by constants and comparisons are supported; other
   rules raise [Unsupported] and are left to [Spec_to_smt]. Since this is an
   independent implementation of the semantics, rules decided by both
   encodings should agree. *)

exception Unsupported of string

let unsupported fmt = ksprintf (fun s -> raise (Unsupported s)) fmt

let z_atom z =
  if Z.lt z Z.zero then app "-" [ atom (Z.to_string (Z.neg z)) ] else atom (Z.to_string z)

let pow2 n = Z.shift_left Z.one n
let num i = z_atom (Z.of_int i)

type width =
  | Word (* 64-bit target word / nativeint *)
  | Host (* 63-bit host int *)

let modulus = function Word -> pow2 64 | Host -> pow2 63
let half w = Z.shift_right (modulus w) 1
let in_range w v = and_ [ app "<=" [ z_atom (Z.neg (half w)); v ]; app "<" [ v; z_atom (half w) ] ]

type ctx =
  { mutable counter : int
  ; mutable decls : string list
  ; mutable facts : Sexp.t list
  ; divisions : (Sexp.t * Sexp.t, Sexp.t * Sexp.t) Hashtbl.Poly.t
  (* variables known to be tagged, written as 2 t + 1 *)
  ; substitutions : (string, Sexp.t) Hashtbl.Poly.t
  }

let fresh ctx prefix =
  ctx.counter <- ctx.counter + 1;
  let name = sprintf "%s_%i" prefix ctx.counter in
  ctx.decls <- name :: ctx.decls;
  atom name

let fact ctx f = ctx.facts <- f :: ctx.facts

let wrap ctx w e =
  let v = fresh ctx "w" and k = fresh ctx "k" in
  fact ctx (eq v (app "-" [ e; app "*" [ k; z_atom (modulus w) ] ]));
  fact ctx (in_range w v);
  v

(* floor (e / 2^n) *)
let floor_pow2 ctx e n =
  let q = fresh ctx "fl" in
  let r = app "-" [ e; app "*" [ z_atom (pow2 n); q ] ] in
  fact ctx (and_ [ app "<=" [ num 0; r ]; app "<" [ r; z_atom (pow2 n) ] ]);
  q

let abs x = ite (app "<" [ x; num 0 ]) (app "-" [ x ]) x

(* Truncated division, constrained only when [defined] *)
let divide ctx a b ~defined =
  Hashtbl.find_or_add ctx.divisions (a, b) ~default:(fun () ->
      let q = fresh ctx "q" and r = fresh ctx "r" in
      fact ctx
        (implies (and_ defined)
           (and_
              [ eq a (app "+" [ app "*" [ q; b ]; r ])
              ; app "<" [ abs r; abs b ]
              ; or_ [ eq r (num 0); eq (app "<" [ r; num 0 ]) (app "<" [ a; num 0 ]) ]
              ]));
      q, r)

let unsigned w v = ite (app "<" [ v; num 0 ]) (app "+" [ v; z_atom (modulus w) ]) v

let cmp (c : Spec.Compare.t) a b =
  match c with
  | EQ -> eq a b
  | NE -> ne a b
  | LT -> app "<" [ a; b ]
  | GT -> app ">" [ a; b ]
  | LE -> app "<=" [ a; b ]
  | GE -> app ">=" [ a; b ]

let var_name (v : _ Spec.Var.t) = Spec.Var.Name.to_string v.name

(* Literal shift amounts *)
let literal_of_host : Spec.Host.expr -> int option = function
  | Lit n -> Some n
  | Natlit n -> Int64.to_int n
  | _ -> None

module Host = struct
  open Spec.Host

  let width = function Int -> Host | Nativeint -> Word

  let rec expr ctx (e : expr) : Sexp.t * Sexp.t list =
    match e with
    | Var v -> atom (var_name v), []
    | Natvar v -> atom (var_name v), []
    | Lit i -> num i, []
    | Natlit i -> z_atom (Z.of_int64 i), []
    | Of_int a -> expr ctx a
    | To_int a ->
      let a, da = expr ctx a in
      wrap ctx Host a, da
    | Unop (Neg, w, a) ->
      let a, da = expr ctx a in
      wrap ctx (width w) (app "-" [ a ]), da
    | Unop (Lnot, _, a) ->
      let a, da = expr ctx a in
      app "-" [ app "-" [ a ]; num 1 ], da
    | Unop (Log2, _, _) -> unsupported "log2"
    | Binop (op, w, a, b) ->
      let w = width w in
      let a, da = expr ctx a in
      let b, db = expr ctx b in
      let v, d =
        match op with
        | Add -> wrap ctx w (app "+" [ a; b ]), []
        | Sub -> wrap ctx w (app "-" [ a; b ]), []
        | Mul -> wrap ctx w (app "*" [ a; b ]), []
        | Div | Mod ->
          let defined = [ ne b (num 0) ] in
          let q, r = divide ctx a b ~defined in
          (match op with Div -> wrap ctx w q | _ -> r), defined
        | Land | Lor | Lxor -> unsupported "host logical operation"
        | Lsl | Lsr | Asr -> unsupported "host shift"
      in
      v, d @ da @ db
    | Compare (_, a, b) ->
      let a, da = expr ctx a in
      let b, db = expr ctx b in
      ite (app "<" [ a; b ]) (num (-1)) (ite (eq a b) (num 0) (num 1)), da @ db
    | Ite (c, a, b) ->
      let a, da = expr ctx a in
      let b, db = expr ctx b in
      let c = cond ctx c in
      ite c a b, [ implies c (and_ da); implies (not_ c) (and_ db) ]

  and cond ctx (c : cond) : Sexp.t =
    match c with
    | Bool true -> true_
    | Bool false -> false_
    | And l -> and_ (List.map l ~f:(cond ctx))
    | Or l -> or_ (List.map l ~f:(cond ctx))
    | Not c -> not_ (cond ctx c)
    (* [Misc.is_power2 n], i.e. [n = 1 lsl Misc.log2 n]: for a host int this
       holds exactly for n = 2^l, 0 <= l <= 61 *)
    | Cmp (EQ, a, Binop (Lsl, Int, Lit 1, Unop (Log2, Int, a')))
      when Poly.equal a a' ->
      let a, _ = expr ctx a in
      or_ (List.init 62 ~f:(fun l -> eq a (z_atom (pow2 l))))
    | Cmp (c, a, b) -> cmp c (fst (expr ctx a)) (fst (expr ctx b))
    | Ucmp (c, a, b) ->
      cmp c (unsigned Word (fst (expr ctx a))) (unsigned Word (fst (expr ctx b)))
    | Divimm _ -> unsupported "divimm"
end

(* A word is either known exactly (in range), or only modulo 2^64. Ring
   operations (+, -, *, lsl) are compatible with reduction modulo 2^64, so
   their results are wrapped only when an operation needs the exact value;
   this keeps products of variables as single nonlinear terms. *)
type value =
  | Exact of Sexp.t
  | Modulo of Sexp.t

let force ctx = function
  | Exact e -> e
  | Modulo e -> wrap ctx Word e

let term = function Exact e | Modulo e -> e

let rec phrase ctx (p : _ Spec.Phrase.t) : value * Sexp.t list =
  match p.kind with
  | Var v ->
    (match Hashtbl.find ctx.substitutions (var_name v) with
     | Some e -> Exact e
     | None -> Exact (atom (var_name v))),
    []
  | Const c ->
    Exact (num (match c with Zero -> 0 | One -> 1 | Three -> 3 | Neg_one -> -1)), []
  | Const_int e | Const_natint e ->
    let e, d = Host.expr ctx e in
    Exact e, d
  | Op1 { kind; p1 } ->
    let a, da = phrase ctx p1 in
    (match kind with
     | Tagi -> Modulo (app "+" [ app "*" [ num 2; term a ]; num 1 ])
     | Untagi -> Exact (floor_pow2 ctx (force ctx a) 1)),
    da
  | Op2 { kind; p1; p2 } ->
    let a, da = phrase ctx p1 in
    let b, db = phrase ctx p2 in
    let v, d =
      match kind with
      | Arith Add -> Modulo (app "+" [ term a; term b ]), []
      | Arith Sub -> Modulo (app "-" [ term a; term b ]), []
      | Arith Mul -> Modulo (app "*" [ term a; term b ]), []
      | Arith Mulhi ->
        Exact (floor_pow2 ctx (app "*" [ force ctx a; force ctx b ]) 64), []
      | Arith ((Div | Mod) as op) ->
        let a = force ctx a and b = force ctx b in
        let defined =
          [ ne b (num 0)
          ; not_ (and_ [ eq a (z_atom (Z.neg (pow2 63))); eq b (num (-1)) ]) ]
        in
        let q, r = divide ctx a b ~defined in
        Exact (match op with Div -> q | _ -> r), defined
      | Shift s ->
        let k =
          match p2.kind with
          | Const One -> 1
          | Const Zero -> 0
          | Const_int e ->
            (match literal_of_host e with
             | Some k -> k
             | None -> unsupported "shift by a non-literal amount")
          | _ -> unsupported "shift by a non-constant amount"
        in
        if k < 0 || k >= 64
        then Exact (num 0), [ false_ ]
        else
          (match s with
           | Lsl -> Modulo (app "*" [ term a; z_atom (pow2 k) ])
           | Asr -> Exact (floor_pow2 ctx (force ctx a) k)
           | Lsr -> Modulo (floor_pow2 ctx (unsigned Word (force ctx a)) k)),
          []
      | Logical _ -> unsupported "logical operation"
      | Compare (K c) ->
        Exact (ite (cmp c (force ctx a) (force ctx b)) (num 1) (num 0)), []
      | Compare_unsigned (K c) ->
        Exact
          (ite (cmp c (unsigned Word (force ctx a)) (unsigned Word (force ctx b)))
             (num 1) (num 0)),
        []
      | Compare _ | Compare_unsigned _ -> unsupported "comparison variable"
    in
    v, d @ da @ db
  | Seq (a, b) ->
    let _, da = phrase ctx a in
    let b, db = phrase ctx b in
    b, da @ db
  | Ite (c, a, b) ->
    let c, dc = phrase ctx c in
    let a, da = phrase ctx a in
    let b, db = phrase ctx b in
    let t = ne (force ctx c) (num 0) in
    Exact (ite t (force ctx a) (force ctx b)),
    dc @ [ implies t (and_ da); implies (not_ t) (and_ db) ]
  | Fcompare _ -> unsupported "float"
  | Load _ -> unsupported "load"
  | Masked _ -> unsupported "masked"

let pre ctx (p : _ Spec.Pre.t) =
  let v p = force ctx (fst (phrase ctx p)) in
  match p with
  | Tagged p -> eq (app "mod" [ v p; num 2 ]) (num 1)
  | Is_bool p -> or_ [ eq (v p) (num 0); eq (v p) (num 1) ]
  | Tagged_bool p -> or_ [ eq (v p) (num 1); eq (v p) (num 3) ]
  | Cmp (c, a, b) -> cmp c (v a) (v b)

(* Same protocol as [Spec_to_smt.verify] with [check_vacuity] and
   [get_values]. Raises [Unsupported]. *)
let verify name (rule : Spec.Rule.t) : string =
  let ctx =
    { counter = 0
    ; decls = []
    ; facts = []
    ; divisions = Hashtbl.Poly.create ()
    ; substitutions = Hashtbl.Poly.create ()
    }
  in
  (* A tagged variable is 2 t + 1 for some 63-bit t *)
  List.iter rule.pre ~f:(function
      | Tagged { kind = Var v; _ } when not (Hashtbl.mem ctx.substitutions (var_name v)) ->
        let t = fresh ctx ("untagged_" ^ var_name v) in
        fact ctx (in_range Host t);
        let tagged = app "+" [ app "*" [ num 2; t ]; num 1 ] in
        (* keeps the variable meaningful in counterexamples *)
        fact ctx (eq (atom (var_name v)) tagged);
        Hashtbl.set ctx.substitutions ~key:(var_name v) ~data:tagged
      | _ -> ());
  let vars = Spec.Rule.all_vars rule in
  let var_decls, var_ranges =
    Map.data vars
    |> List.map ~f:(fun (Spec.Var.T v) ->
        let name = var_name v in
        let range =
          match v.kind with
          | Int | Natconst -> in_range Word (atom name)
          | Const -> in_range Host (atom name)
          | Cmp | Fcmp | Float -> unsupported "variable kind"
        in
        name, range)
    |> List.unzip
  in
  let guard = Host.cond ctx rule.guard in
  let pre = List.map rule.pre ~f:(pre ctx) in
  let input, input_defined = phrase ctx rule.input in
  let output, output_defined = phrase ctx rule.output in
  let input = force ctx input and output = force ctx output in
  let relation =
    match rule.relation with
    | Equal -> eq input output
    | Truthiness -> eq (ne output (num 0)) (ne input (num 1))
    | Agree_on _ -> unsupported "relation"
  in
  let assumptions =
    and_ (var_ranges @ (guard :: pre) @ input_defined @ List.rev ctx.facts)
  in
  let lines =
    [ "(push 1)"; sprintf "(echo \"verifying %s\")" name ]
    @ List.map (var_decls @ List.rev ctx.decls) ~f:(sprintf "(declare-const %s Int)")
    @ [ "(push 1)"
      ; "(echo \"assumptions satisfiable?\")"
      ; Sexp.to_string (app "assert" [ assumptions ])
      ; "(check-sat)"
      ; "(pop 1)"
      ; Sexp.to_string (app "assert" [ assumptions ])
      ; Sexp.to_string
          (app "assert" [ not_ (and_ (output_defined @ [ relation ])) ])
      ; "(check-sat)"
      ]
    @ (if List.is_empty var_decls then []
       else [ sprintf "(get-value (%s))" (String.concat ~sep:" " var_decls) ])
    @ [ "(pop 1)" ]
  in
  String.concat ~sep:"\n" lines ^ "\n"

(* Writes the supported rules, one file each; returns the names of the
   unsupported ones *)
let write_dir ~dirname ~rules =
  List.filter_mapi rules ~f:(fun index (name, rule) ->
      match verify name rule with
      | text ->
        let base =
          String.map name ~f:(fun c ->
              if Char.is_alphanum c || Char.equal c '_' || Char.equal c '-' then c else '_')
        in
        Out_channel.write_all (dirname ^/ sprintf "%03i_%s.smt2" index base) ~data:text;
        None
      | exception Unsupported reason -> Some (name, reason))
