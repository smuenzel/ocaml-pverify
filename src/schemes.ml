open! Core

class rule_helper = object (self)
  val vars = String.Table.create ()
  val vars_cmp = String.Table.create ()
  val vars_const = String.Table.create ()
  val debugs = String.Table.create ()

  method var_cmp name =
    match Hashtbl.find vars_cmp name with 
    | Some var -> var
    | None ->
      let var = Spec.Make.var_cmp name in
      Hashtbl.add_exn vars_cmp ~key:name ~data:var;
      var

  method var name =
    match Hashtbl.find vars name with 
    | Some var -> var
    | None ->
      let var = Spec.Make.var name in
      Hashtbl.add_exn vars ~key:name ~data:var;
      var

  method var_s name = fst (self#var name)
  method var_d name = snd (self#var name)

  method debug name =
    match Hashtbl.find debugs name with 
    | Some debug -> debug
    | None ->
      let name_s = Spec.Debug.Source.Name.of_string name in
      let debug = 
        Spec.Debug.Source.Set name_s
      , Spec.Debug.Destination.From_source name_s
      in
      Hashtbl.add_exn debugs ~key:name ~data:debug;
      debug

  method dbg_s name = fst (self#debug name)
  method dbg_d name = snd (self#debug name)

  method var_const name =
    match Hashtbl.find vars_const name with
    | Some var -> var
    | None ->
      let var = Spec.Make.var_const name in
      Hashtbl.add_exn vars_const ~key:name ~data:var;
      var

  method make ?(guard = []) input output =
    { Spec.Rule.
      input
    ; output
    ; guard
    }
end

open Spec.Make

let tag_xor =
  let r = new rule_helper in
  r#make
    begin
      op2 (Logical Or)
        (op2 (Logical Xor)
           (op1 Tagi (r#var_s "left") Anon)
           (op1 Tagi (r#var_s "right") Anon)
           (r#dbg_s "dbg_xor")
        )
        (const One Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg_xor" in
      op1 Tagi (op2 (Logical Xor) (r#var_d "left") (r#var_d "right") dbg) dbg
    end

let tag_or =
  let r = new rule_helper in
  r#make
    begin
      op2 (Logical Or)
        (op1 Tagi (r#var_s "left") Anon)
        (op1 Tagi (r#var_s "right") Anon)
        (r#dbg_s "dbg_op")
    end
    begin
      let dbg = r#dbg_d "dbg_op" in
      op1 Tagi (op2 (Logical Or) (r#var_d "left") (r#var_d "right") dbg) dbg
    end

let tag_and =
  let r = new rule_helper in
  r#make
    begin
      op2 (Logical And)
        (op1 Tagi (r#var_s "left") Anon)
        (op1 Tagi (r#var_s "right") Anon)
        (r#dbg_s "dbg_op")
    end
    begin
      let dbg = r#dbg_d "dbg_op" in
      op1 Tagi (op2 (Logical And) (r#var_d "left") (r#var_d "right") dbg) dbg
    end

let nested_compare =
  let r = new rule_helper in
  r#make
    begin
      op2 (Compare (V (r#var_cmp "cmp_outer")))
        (op1 Tagi
           (op2 (Compare (V (r#var_cmp "cmp_inner")))
              (r#var_s "left")
              (r#var_s "right")
              (r#dbg_s "dbg")
           )
           Anon
        )
        (const One Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg" in
      op2 (Compare (V (r#var_cmp "cmp_outer")))
        (op2 (Compare (V (r#var_cmp "cmp_inner")))
           (r#var_d "left")
           (r#var_d "right")
           dbg
        )
        (const Zero dbg)
        dbg
    end

let nested_nequal_zero =
  let r = new rule_helper in
  r#make
    begin
      op2 (Compare (K NE))
        (op2 (Compare (V (r#var_cmp "cmp_inner")))
           (r#var_s "left")
           (r#var_s "right")
           (r#dbg_s "dbg")
        )
        (const Zero Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg" in
      (op2 (Compare (V (r#var_cmp "cmp_inner")))
         (r#var_d "left")
         (r#var_d "right")
         dbg
      )
    end

let nested_nequal_one_tagged =
  let r = new rule_helper in
  r#make
    begin
      op2 (Compare (K NE))
        (op1 Tagi 
           (op2 (Compare (V (r#var_cmp "cmp_inner")))
              (r#var_s "left")
              (r#var_s "right")
              (r#dbg_s "dbg")
           )
           Anon
        )
        (const One Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg" in
      (op2 (Compare (V (r#var_cmp "cmp_inner")))
         (r#var_d "left")
         (r#var_d "right")
         dbg
      )
    end

let nested_equal_one =
  let r = new rule_helper in
  r#make
    begin
      op2 (Compare (K EQ))
        (op2 (Compare (V (r#var_cmp "cmp_inner")))
           (r#var_s "left")
           (r#var_s "right")
           (r#dbg_s "dbg")
        )
        (const One Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg" in
      (op2 (Compare (V (r#var_cmp "cmp_inner")))
         (r#var_d "left")
         (r#var_d "right")
         dbg
      )
    end

let nested_equal_three_tagged =
  let r = new rule_helper in
  r#make
    begin
      op2 (Compare (K EQ))
        (op1 Tagi 
           (op2 (Compare (V (r#var_cmp "cmp_inner")))
              (r#var_s "left")
              (r#var_s "right")
              (r#dbg_s "dbg")
           )
           Anon
        )
        (const Three Spec.Debug.Source.Anon)
        Anon
    end
    begin
      let dbg = r#dbg_d "dbg" in
      (op2 (Compare (V (r#var_cmp "cmp_inner")))
         (r#var_d "left")
         (r#var_d "right")
         dbg
      )
    end

(* From [Cmm_helpers.untag_int]:
   {[
     | Cop(Cor, [Cop(Casr, [c; Cconst_int (n, _)], _); Cconst_int (1, _)], _)
       when n > 0 && n < size_int * 8 ->
         Cop(Casr, [c; Cconst_int (n+1, dbg)], dbg)
   ]}
   The result of the [Cor] is untagged, i.e. shifted right by one. *)
let untag_or_shift shift =
  let r = new rule_helper in
  let n = r#var_const "n" in
  r#make
    ~guard:
      [ guard GT (Const_var (n, 0)) (Literal 0)
      ; guard LT (Const_var (n, 0)) (Word_bits 0)
      ]
    begin
      op1 Untagi
        (op2 (Logical Or)
           (op2 (Shift shift)
              (r#var_s "c")
              (const_var n Spec.Debug.Source.Anon)
              Anon
           )
           (const One Spec.Debug.Source.Anon)
           Anon
        )
        (r#dbg_s "dbg")
    end
    begin
      let dbg = r#dbg_d "dbg" in
      op2 (Shift shift) (r#var_d "c") (const_var ~offset:1 n dbg) dbg
    end

let untag_or_asr = untag_or_shift Asr
let untag_or_lsr = untag_or_shift Lsr

let example_spec = tag_xor

let all =
  [ "tag-xor", tag_xor
  ; "tag-and", tag_and
  ; "tag-or", tag_or
  ; "nested-compare", nested_compare
  ; "nested-nequal-zero", nested_nequal_zero
  ; "nested-nequal-one-tagged", nested_nequal_one_tagged
  ; "nested-equal-one", nested_equal_one
  ; "nested-equal-three-tagged", nested_equal_three_tagged
  ; "untag-or-asr", untag_or_asr
  ; "untag-or-lsr", untag_or_lsr
  ]
  |> String.Map.of_alist_exn


