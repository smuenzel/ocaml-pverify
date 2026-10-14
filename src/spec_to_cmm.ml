open! Core

module Cmm = struct
  (* Constructors starting with "X" are pseudo-constructors not present in actual Cmm *)
  type expression =
    | Cop of operation * expression list * debuginfo
    | Cconst_int of int * debuginfo
    | Csequence of expression * expression
    | Cifthenelse of expression * expression * expression * debuginfo
    | Xexp_var of string
    | Xconst_int of string * debuginfo (* Cconst_int (host expression, dbg) *)
    | Xconst_natint of string * debuginfo (* Cconst_natint (host expression, dbg) *)
    | Xmasked of string * expression (* some value agreeing with the expression on a mask *)
  and operation =
    | Caddi
    | Csubi
    | Cmuli
    | Cmulhi
    | Cdivi
    | Cmodi
    | Cand
    | Cor
    | Cxor
    | Clsl
    | Clsr
    | Casr
    | Ccmpi of string
    | Ccmpa of string
    | Xccmpf of string * string * string
    | Xload of string
  and debuginfo =
    | Xdbg_anon
    | Xdbg_var of string
  [@@deriving sexp_of]

  let rec size_of_expression = function
    | Cop (_, elist, _) ->
      1 + List.sum (module Int) ~f:size_of_expression elist
    | Csequence (a, b) -> size_of_expression a + size_of_expression b
    | Cifthenelse (a, b, c, _) ->
      1 + size_of_expression a + size_of_expression b + size_of_expression c
    | Xmasked (_, e) -> size_of_expression e
    | Cconst_int _
    | Xexp_var _
    | Xconst_int _
    | Xconst_natint _ ->
      0

  let rec count_heads = function
    | Cop (_, elist, _) ->
      1 + List.sum (module Int) ~f:count_heads elist
    | Csequence (a, b) -> 1 + count_heads a + count_heads b
    | Cifthenelse (a, b, c, _) ->
      1 + count_heads a + count_heads b + count_heads c
    | Xmasked (_, e) -> count_heads e
    | Cconst_int _
    | Xexp_var _
    | Xconst_int _
    | Xconst_natint _ ->
      1

  module Print = struct

    let rec print_expression = function
      | Cop (Xccmpf (cmp, f1, f2), [], debuginfo) ->
        sprintf "Cop (Ccmpf %s,[%s; %s],%s)" cmp f1 f2 (print_debuginfo debuginfo)
      | Cop (operation, explist, debuginfo) ->
        let op = print_operation operation in
        let explist =
          List.map ~f:print_expression explist
          |> String.concat ~sep:"; "
        in
        let dbg = print_debuginfo debuginfo in
        sprintf "Cop (%s,[%s],%s)"
          op explist dbg
      | Cconst_int (i, debuginfo) ->
        let dbg = print_debuginfo debuginfo in
        sprintf "Cconst_int (%i,%s)" i dbg
      | Csequence (a, b) ->
        sprintf "Csequence (%s, %s)" (print_expression a) (print_expression b)
      | Cifthenelse (a, b, c, debuginfo) ->
        sprintf "Cifthenelse (%s, %s, %s, %s)"
          (print_expression a) (print_expression b) (print_expression c)
          (print_debuginfo debuginfo)
      | Xexp_var s -> s
      | Xconst_int (value, debuginfo) ->
        sprintf "Cconst_int (%s,%s)" value (print_debuginfo debuginfo)
      | Xconst_natint (value, debuginfo) ->
        sprintf "Cconst_natint (%s,%s)" value (print_debuginfo debuginfo)
      | Xmasked (mask, e) ->
        sprintf "(* agrees on %s with *) %s" mask (print_expression e)
    and print_operation = function
      | Caddi
      | Csubi
      | Cmuli
      | Cmulhi
      | Cdivi
      | Cmodi
      | Cand
      | Cor
      | Cxor
      | Clsl
      | Clsr
      | Casr
        as op ->
        Sexp.to_string ([%sexp_of: operation] op)
      | Ccmpi cmp -> sprintf "Ccmpi %s" cmp
      | Ccmpa cmp -> sprintf "Ccmpa %s" cmp
      | Xccmpf (cmp, _, _) -> sprintf "Ccmpf %s" cmp
      | Xload chunk -> sprintf "Cload %s" chunk
    and print_debuginfo = function
      | Xdbg_anon -> "_"
      | Xdbg_var s -> s


  end

end

let expand_debug : _ -> Cmm.debuginfo = function
  | None -> Xdbg_anon
  | Some var -> Xdbg_var var

let expr_of_const ~dbg : Spec.Const.t -> Cmm.expression = function
  | Zero -> Cconst_int (0, expand_debug dbg)
  | One  -> Cconst_int (1, expand_debug dbg)
  | Three -> Cconst_int (3, expand_debug dbg)
  | Neg_one -> Cconst_int (-1, expand_debug dbg)

let expr_of_var var =
  Cmm.Xexp_var (Spec.Var.Name.to_string var.Spec.Var.name)

let var_name (v : _ Spec.Var.t) = Spec.Var.Name.to_string v.name

let print_compare : Spec.Compare.t -> string = function
  | EQ -> "Ceq"
  | NE -> "Cne"
  | LT -> "Clt"
  | GT -> "Cgt"
  | LE -> "Cle"
  | GE -> "Cge"

let rec print_icmp : Spec.Icmp.t -> string = function
  | K c -> print_compare c
  | V v -> var_name v
  | Negate c -> sprintf "(negate_integer_comparison %s)" (print_icmp c)
  | Swap c -> sprintf "(swap_integer_comparison %s)" (print_icmp c)

let rec print_fcmp : Spec.Fcmp.t -> string = function
  | K c ->
    (match c with
     | FEQ -> "CFeq" | FNEQ -> "CFneq" | FLT -> "CFlt" | FNLT -> "CFnlt"
     | FGT -> "CFgt" | FNGT -> "CFngt" | FLE -> "CFle" | FNLE -> "CFnle"
     | FGE -> "CFge" | FNGE -> "CFnge")
  | V v -> var_name v
  | Negate c -> sprintf "(negate_float_comparison %s)" (print_fcmp c)
  | Swap c -> sprintf "(swap_float_comparison %s)" (print_fcmp c)

(* Host expressions, printed as OCaml *)
module Host = struct
  open Spec.Host

  let binop (op : Binop.t) (w : width) =
    match w, op with
    | Int, Add -> "+" | Int, Sub -> "-" | Int, Mul -> "*" | Int, Div -> "/"
    | Int, Mod -> "mod" | Int, Land -> "land" | Int, Lor -> "lor"
    | Int, Lxor -> "lxor" | Int, Lsl -> "lsl" | Int, Lsr -> "lsr"
    | Int, Asr -> "asr"
    | Nativeint, Add -> "Nativeint.add" | Nativeint, Sub -> "Nativeint.sub"
    | Nativeint, Mul -> "Nativeint.mul" | Nativeint, Div -> "Nativeint.div"
    | Nativeint, Mod -> "Nativeint.rem" | Nativeint, Land -> "Nativeint.logand"
    | Nativeint, Lor -> "Nativeint.logor" | Nativeint, Lxor -> "Nativeint.logxor"
    | Nativeint, Lsl -> "Nativeint.shift_left"
    | Nativeint, Lsr -> "Nativeint.shift_right_logical"
    | Nativeint, Asr -> "Nativeint.shift_right"

  let rec expr : expr -> string = function
    | Var v -> var_name v
    | Natvar v -> var_name v
    | Lit i -> if i < 0 then sprintf "(%i)" i else Int.to_string i
    | Natlit i -> sprintf "%Ldn" i
    | Unop (Neg, Int, a) -> sprintf "(- %s)" (expr a)
    | Unop (Neg, Nativeint, a) -> sprintf "(Nativeint.neg %s)" (expr a)
    | Unop (Lnot, Int, a) -> sprintf "(lnot %s)" (expr a)
    | Unop (Lnot, Nativeint, a) -> sprintf "(Nativeint.lognot %s)" (expr a)
    | Unop (Log2, _, a) -> sprintf "(Misc.log2 %s)" (expr a)
    | Binop (op, Int, a, b) -> sprintf "(%s %s %s)" (expr a) (binop op Int) (expr b)
    | Binop (op, Nativeint, a, b) ->
      sprintf "(%s %s %s)" (binop op Nativeint) (expr a) (expr b)
    | Of_int a -> sprintf "(Nativeint.of_int %s)" (expr a)
    | To_int a -> sprintf "(Nativeint.to_int %s)" (expr a)
    | Compare (Int, a, b) -> sprintf "(Int.compare %s %s)" (expr a) (expr b)
    | Compare (Nativeint, a, b) ->
      sprintf "(Nativeint.compare %s %s)" (expr a) (expr b)
    | Ite (c, a, b) -> sprintf "(if %s then %s else %s)" (cond c) (expr a) (expr b)

  and cond : cond -> string = function
    | Bool b -> Bool.to_string b
    | And [] -> "true"
    | And l -> List.map l ~f:cond |> String.concat ~sep:" && "
    | Or [] -> "false"
    | Or l -> sprintf "(%s)" (List.map l ~f:cond |> String.concat ~sep:" || ")
    | Not c -> sprintf "not (%s)" (cond c)
    | Cmp (c, a, b) ->
      let op =
        match c with
        | EQ -> "=" | NE -> "<>" | LT -> "<" | GT -> ">" | LE -> "<=" | GE -> ">="
      in
      sprintf "%s %s %s" (expr a) op (expr b)
    | Ucmp (c, a, b) ->
      sprintf "ucompare %s %s %s 0" (expr a) (expr b)
        (match c with
         | EQ -> "=" | NE -> "<>" | LT -> "<" | GT -> ">" | LE -> "<=" | GE -> ">=")
    | Divimm { d; m; p } ->
      sprintf "(%s, %s) = divimm_parameters %s" (expr m) (expr p) (expr d)
end

let rec cmm_of_phrase ~combine_debug ~dbg (phrase : _ Spec.Phrase.t) =
  let { Spec.Phrase.
        kind
      ; debug
      } = phrase
  in
  let dbg = combine_debug ~inherited:dbg ~current:debug in
  let recurse = cmm_of_phrase ~combine_debug ~dbg in
  match kind with
  | Var v -> expr_of_var v
  | Const c -> expr_of_const ~dbg c
  | Const_int e -> Xconst_int (Host.expr e, expand_debug dbg)
  | Const_natint e -> Xconst_natint (Host.expr e, expand_debug dbg)
  | Op1 op1 -> expr_of_op1 ~combine_debug ~dbg op1
  | Op2 op2 -> expr_of_op2 ~combine_debug ~dbg op2
  | Fcompare { cmp; f1; f2 } ->
    Cop (Xccmpf (print_fcmp cmp, var_name f1, var_name f2), [], expand_debug dbg)
  | Seq (a, b) -> Csequence (recurse a, recurse b)
  | Ite (a, b, c) -> Cifthenelse (recurse a, recurse b, recurse c, expand_debug dbg)
  | Load { chunk; endian; addr } ->
    let chunk =
      Sexp.to_string (Spec.Chunk.sexp_of_t chunk)
      ^ (match endian with Little -> "" | Big -> " (big-endian)")
    in
    Cop (Xload chunk, [ recurse addr ], expand_debug dbg)
  | Masked { mask; arg } -> Xmasked (Host.expr mask, recurse arg)

and expr_of_op2 ~combine_debug ~dbg op2 =
  let { Spec.Binary_op.
        kind
      ; p1
      ; p2
      } = op2
  in
  let (cmm_op : Cmm.operation) =
    match kind with
    | Shift Lsl -> Clsl
    | Shift Lsr -> Clsr
    | Shift Asr -> Casr
    | Logical Or -> Cor
    | Logical And -> Cand
    | Logical Xor -> Cxor
    | Arith Add -> Caddi
    | Arith Sub -> Csubi
    | Arith Mul -> Cmuli
    | Arith Mulhi -> Cmulhi
    | Arith Div -> Cdivi
    | Arith Mod -> Cmodi
    | Compare cmp -> Ccmpi (print_icmp cmp)
    | Compare_unsigned cmp -> Ccmpa (print_icmp cmp)
  in
  let p1 = cmm_of_phrase ~combine_debug ~dbg p1 in
  let p2 = cmm_of_phrase ~combine_debug ~dbg p2 in
  Cmm.Cop (cmm_op, [p1;p2], expand_debug dbg)

and expr_of_op1 ~combine_debug ~dbg op1 =
  let { Spec.Unary_op.
        kind
      ; p1
      } = op1
  in
  let p1 = cmm_of_phrase ~combine_debug ~dbg p1 in
  match kind with
  | Tagi ->
    Cmm.Cop
      (Caddi
      , [ Cop (Clsl, [p1; Cconst_int (1,expand_debug dbg)], expand_debug dbg)
        ; Cconst_int (1, expand_debug dbg)
        ]
      , expand_debug dbg
      )
  | Untagi ->
    Cmm.Cop
      (Casr
      , [ p1; Cconst_int (1, expand_debug dbg) ]
      , expand_debug dbg
      )


let combine_debug_source ~inherited:_ ~current =
  match (current : Spec.Debug.Source.t) with
  | Anon -> None
  | Set name -> Some (Spec.Debug.Source.Name.to_string name)

let combine_debug_destination ~inherited ~current =
  match (current : Spec.Debug.Destination.t) with
  | Inherit -> inherited
  | From_source name -> Some (Spec.Debug.Source.Name.to_string name)

let pattern_cmm_of_spec spec : Cmm.expression =
  cmm_of_phrase ~combine_debug:combine_debug_source ~dbg:None spec

let expression_cmm_of_spec spec : Cmm.expression =
  cmm_of_phrase ~combine_debug:combine_debug_destination ~dbg:None spec

let to_match_case ?name rule =
  let { Spec.Rule. input; output; guard; pre = _; relation = _ } = rule in
  let input_cmm = pattern_cmm_of_spec input in
  let input = Cmm.Print.print_expression input_cmm in
  let output_cmm = expression_cmm_of_spec output in
  let output = Cmm.Print.print_expression output_cmm in
  let size_input = Cmm.size_of_expression input_cmm in
  let heads_input = Cmm.count_heads input_cmm in
  let size_output = Cmm.size_of_expression output_cmm in
  let heads_output = Cmm.count_heads output_cmm in
  let maybe_name =
    match name with
    | None ->      "  (* "
    | Some name -> "  (* " ^ name ^ "\n     "
  in
  let when_ =
    match guard with
    | Bool true | And [] -> ""
    | guard -> sprintf "\n    when %s" (Host.cond guard)
  in
  String.concat ~sep:"\n"
    [ sprintf "| %s%s ->" input when_
    ; sprintf "%s Size: (%i ops, %i heads) in -> (%i ops, %i heads) out *)"
        maybe_name size_input heads_input size_output heads_output
    ; sprintf "  %s" output
    ]
