open! Core

module Cmm = struct
  (* Constructors starting with "X" are pseudo-constructors not present in actual Cmm *)
  type expression =
    | Cop of operation * expression list * debuginfo
    | Cconst_int of int * debuginfo
    | Xexp_var of string
  and operation =
    | Caddi
    | Cand
    | Cor
    | Cxor
    | Clsl
    | Clsr
    | Casr
    | Ccmpi of integer_comparison
  and integer_comparison =
    | Ceq
    | Cne
    | Clt
    | Cgt
    | Cle
    | Cge
  and debuginfo =
    | Xdbg_anon
    | Xdbg_var of string
  [@@deriving sexp]

  let rec size_of_expression = function
    | Cop (_, elist, _) ->
      1 + List.sum (module Int) ~f:size_of_expression elist
    | Cconst_int _ ->
      0
    | Xexp_var _ ->
      0

  let rec count_heads = function
    | Cop (_, elist, _) ->
      1 + List.sum (module Int) ~f:count_heads elist
    | Cconst_int _ ->
      1
    | Xexp_var _ ->
      1

  module Print = struct

    let rec print_expression = function
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
      | Xexp_var s -> s
    and print_operation op =
      Sexp.to_string ([%sexp_of: operation] op)
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
  | Neg_one -> Cconst_int (-1, expand_debug dbg)

let expr_of_var var =
  Cmm.Xexp_var (Spec.Var.Name.to_string var.Spec.Var.name)

let compare_of_compare : Spec.Binary_op.Kind.Compare.t -> Cmm.integer_comparison = function
  | EQ -> Ceq
  | NE -> Cne
  | LT -> Clt
  | GT -> Cgt
  | LE -> Cle
  | GE -> Cge

let rec cmm_of_phrase ~combine_debug ~dbg (phrase : _ Spec.Phrase.t) =
  let { Spec.Phrase.
        kind
      ; debug
      } = phrase 
  in
  let dbg = combine_debug ~inherited:dbg ~current:debug in
  match kind with
  | Var v -> expr_of_var v
  | Const c -> expr_of_const ~dbg c
  | Op1 op1 -> expr_of_op1 ~combine_debug ~dbg op1
  | Op2 op2 -> expr_of_op2 ~combine_debug ~dbg op2

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
    | Compare cmp ->
      Ccmpi (compare_of_compare cmp)
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
  let Tagi = kind in
  let p1 = cmm_of_phrase ~combine_debug ~dbg p1 in
  Cmm.Cop 
    (Caddi
    , [ Cop (Clsl, [p1; Cconst_int (1,expand_debug dbg)], expand_debug dbg)
      ; Cconst_int (1, expand_debug dbg)
      ]
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
  let { Spec.Rule. input; output } = rule in
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
  String.concat ~sep:"\n"
    [ sprintf "| %s ->" input
    ; sprintf "%s Size: (%i ops, %i heads) in -> (%i ops, %i heads) out *)"
        maybe_name size_input heads_input size_output heads_output
    ; sprintf "  %s" output
    ]



