open! Core

(* The rewrites performed by the smart constructors in the compiler's
   asmcomp/cmm_helpers.ml and asmcomp/cmmgen.ml (OCaml 5.5).

   Each rule states, for one match case of a helper, that the helper's
   result (the output) refines its specification applied to the matched
   pattern (the input). Calls to other helpers in an output are replaced by
   those helpers' specifications, so every helper is verified separately:
   - [add_const c n], [add_int], [offset_addr]   -> c + n
   - [sub_int]                                   -> c1 - c2
   - [mul_int], [lsl_int], [lsr_int], [asr_int]  -> the Cmm operation
   - [tag_int], [int_const]                      -> (x lsl 1) + 1
   - [untag_int]                                 -> x asr 1
   - [ignore_low_bit_int x] / [low_32 x]         -> some value agreeing with x
                                                    except on bit 0 / on the
                                                    low 32 bits
   - [natint_const_untagged n]                   -> Cconst_natint n

   Not covered: cases that only raise or introduce control flow / bindings
   (Clet, Ccatch, Cexit), stores, allocation and object layout, and the
   compilation of switches. *)

open Spec

let var_int name = { Var. name = Var.Name.of_string name; kind = Int }
let var_float name = { Var. name = Var.Name.of_string name; kind = Float }
let var_cmp name = { Var. name = Var.Name.of_string name; kind = Cmp }
let var_fcmp name = { Var. name = Var.Name.of_string name; kind = Fcmp }

(* Host constants *)
let cv name = Host.Var { Var. name = Var.Name.of_string name; kind = Const }
let nv name = Host.Natvar { Var. name = Var.Name.of_string name; kind = Natconst }

(* Native integer masks *)
let all_but_low_bit = Host.Natlit (-2L)
let all_but_high_bit = Host.Binop (Lsr, Nativeint, Natlit (-1L), Natlit 1L)
let low_32_bits = Host.Natlit 0xFFFFFFFFL

module Build (D : sig type t val anon : t end) = struct
  let mk kind = { Phrase. kind; debug = D.anon }
  let v name = mk (Var (var_int name))
  let ci e = mk (Const_int e)
  let cn e = mk (Const_natint e)
  let int n = ci (Host.Lit n)
  let op1 kind p1 = mk (Op1 { kind; p1 })
  let op2 kind p1 p2 = mk (Op2 { kind; p1; p2 })
  let tag a = op1 Tagi a
  let untag a = op1 Untagi a
  let add = op2 (Arith Add)
  let sub = op2 (Arith Sub)
  let mul = op2 (Arith Mul)
  let mulhi = op2 (Arith Mulhi)
  let div = op2 (Arith Div)
  let mod_ = op2 (Arith Mod)
  let and_ = op2 (Logical And)
  let or_ = op2 (Logical Or)
  let xor = op2 (Logical Xor)
  let lsl_ = op2 (Shift Lsl)
  let lsr_ = op2 (Shift Lsr)
  let asr_ = op2 (Shift Asr)
  let cmp c = op2 (Compare c)
  let cmpa c = op2 (Compare_unsigned c)
  let fcmp cmp f1 f2 = mk (Fcompare { cmp; f1 = var_float f1; f2 = var_float f2 })
  let seq a b = mk (Seq (a, b))
  let ite c a b = mk (Ite (c, a, b))
  let load ?(endian = Endian.Little) chunk addr = mk (Load { chunk; endian; addr })
  let masked mask arg = mk (Masked { mask; arg })
end

module I = Build (struct type t = Debug.Source.t let anon = Debug.Source.Anon end)
module O = Build (struct type t = Debug.Destination.t let anon = Debug.Destination.Inherit end)

let rules = Queue.create ()

let rule ?(guard = Host.Bool true) ?(pre = []) ?(relation = Relation.Equal)
    name input output =
  Queue.enqueue rules (name, { Rule. input; output; guard; pre; relation })

module M = Host.Misc

let () =
  let open Host.O in
  let n = cv "n" and x = cv "x" in
  (* add_no_overflow n x c (l.131), called with [no_overflow_add n x] *)
  let spec = I.(add (add (v "c") (ci n)) (ci x)) in
  rule "add_no_overflow:131/zero"
    ~guard:(M.no_overflow_add n x && n + x = i 0) spec (O.v "c");
  rule "add_no_overflow:131/add"
    ~guard:(M.no_overflow_add n x && n + x <> i 0) spec O.(add (v "c") (ci (n + x)));
  (* add_const c n (l.135) *)
  rule "add_const:136/zero" ~guard:(n = i 0) I.(add (v "c") (ci n)) (O.v "c");
  rule "add_const:138/const"
    ~guard:(n <> i 0 && M.no_overflow_add x n) I.(add (ci x) (ci n)) O.(ci (x + n));
  rule "add_const:139/add-const-left"
    ~guard:(n <> i 0 && M.no_overflow_add n x)
    I.(add (add (ci x) (v "c")) (ci n))
    O.(add (add (v "c") (ci n)) (ci x));
  rule "add_const:142/add-const-right"
    ~guard:(n <> i 0 && M.no_overflow_add n x)
    I.(add (add (v "c") (ci x)) (ci n))
    O.(add (add (v "c") (ci n)) (ci x));
  rule "add_const:145/sub-const-left"
    ~guard:(n <> i 0 && M.no_overflow_add n x)
    I.(add (sub (ci x) (v "c")) (ci n))
    O.(sub (ci (n + x)) (v "c"));
  rule "add_const:147/sub-const-right"
    ~guard:(n <> i 0 && M.no_overflow_sub n x)
    I.(add (sub (v "c") (ci x)) (ci n))
    O.(add (v "c") (ci (n - x)))

let () =
  (* offset_addr c1 c2 (l.154): Cadda is address addition *)
  let n1 = cv "n1" in
  rule "offset_addr:156/zero" I.(add (v "c") (int 0)) (O.v "c");
  rule "offset_addr:157/zero-natint" I.(add (v "c") (cn (Natlit 0L))) (O.v "c");
  rule "offset_addr:158/add-const"
    I.(add (add (v "c") (ci n1)) (v "c2"))
    O.(add (v "c") (add (v "c2") (ci n1)))

let () =
  let open Host.O in
  (* add_int c1 c2 (l.162) *)
  let n = cv "n" in
  rule "add_int:164/const-left" I.(add (ci n) (v "c")) O.(add (v "c") (ci n));
  rule "add_int:164/const-right" I.(add (v "c") (ci n)) O.(add (v "c") (ci n));
  rule "add_int:166/add-left"
    I.(add (add (v "c1") (ci n)) (v "c2"))
    O.(add (add (v "c1") (v "c2")) (ci n));
  rule "add_int:168/add-right"
    I.(add (v "c1") (add (v "c2") (ci n)))
    O.(add (add (v "c1") (v "c2")) (ci n));
  (* sub_int c1 c2 (l.173) *)
  rule "sub_int:175/const" ~guard:(n <> min_int)
    I.(sub (v "c1") (ci n)) O.(add (v "c1") (ci (neg n)));
  rule "sub_int:177/add-right" ~guard:(n <> min_int)
    I.(sub (v "c1") (add (v "c2") (ci n)))
    O.(add (sub (v "c1") (v "c2")) (ci (neg n)));
  rule "sub_int:179/add-left"
    I.(sub (add (v "c1") (ci n)) (v "c2"))
    O.(add (sub (v "c1") (v "c2")) (ci n))

let () =
  let open Host.O in
  (* lsl_int c1 c2 (l.184) *)
  let n1 = cv "n1" and n2 = cv "n2" in
  rule "lsl_int:186/lsl-lsl"
    ~guard:(n1 > i 0 && n2 > i 0 && n1 + n2 < i 64)
    I.(lsl_ (lsl_ (v "c") (ci n1)) (ci n2))
    O.(lsl_ (v "c") (ci (n1 + n2)));
  rule "lsl_int:189/lsl-add"
    ~guard:(M.no_overflow_lsl n1 n2)
    I.(lsl_ (add (v "c1") (ci n1)) (ci n2))
    O.(add (lsl_ (v "c1") (ci n2)) (ci (n1 lsl n2)))

let () =
  let open Host.O in
  (* mul_int c1 c2 (l.199); mult_power2 c n = lsl_int c (log2 n) *)
  let n = cv "n" and k = cv "k" in
  rule "mul_int:201/zero-right" I.(mul (v "c") (int 0)) O.(seq (v "c") (int 0));
  rule "mul_int:201/zero-left" I.(mul (int 0) (v "c")) O.(seq (v "c") (int 0));
  rule "mul_int:203/one-right" I.(mul (v "c") (int 1)) (O.v "c");
  rule "mul_int:203/one-left" I.(mul (int 1) (v "c")) (O.v "c");
  rule "mul_int:205/minus-one-right" I.(mul (v "c") (int (-1))) O.(sub (int 0) (v "c"));
  rule "mul_int:205/minus-one-left" I.(mul (int (-1)) (v "c")) O.(sub (int 0) (v "c"));
  rule "mul_int:207/power2-right" ~guard:(M.is_power2 n)
    I.(mul (v "c") (ci n)) O.(lsl_ (v "c") (ci (log2 n)));
  rule "mul_int:208/power2-left" ~guard:(M.is_power2 n)
    I.(mul (ci n) (v "c")) O.(lsl_ (v "c") (ci (log2 n)));
  rule "mul_int:209/add-const-right" ~guard:(M.no_overflow_mul n k)
    I.(mul (add (v "c") (ci n)) (ci k))
    O.(add (mul (v "c") (ci k)) (ci (n * k)));
  rule "mul_int:210/add-const-left" ~guard:(M.no_overflow_mul n k)
    I.(mul (ci k) (add (v "c") (ci n)))
    O.(add (mul (v "c") (ci k)) (ci (n * k)))

let () =
  let open Host.O in
  let n = cv "n" in
  (* ignore_low_bit_int (l.217): the result may differ in bit 0 *)
  rule "ignore_low_bit_int:218/tag-shift" ~relation:(Agree_on all_but_low_bit)
    ~guard:(n > i 0)
    I.(add (lsl_ (v "c") (ci n)) (int 1)) O.(lsl_ (v "c") (ci n));
  rule "ignore_low_bit_int:222/or-one" ~relation:(Agree_on all_but_low_bit)
    I.(or_ (v "c") (int 1)) (O.v "c");
  (* ignore_high_bit_int (l.226): the result may differ in bit 63 *)
  rule "ignore_high_bit_int:227/tag-untag" ~relation:(Agree_on all_but_high_bit)
    I.(asr_ (lsl_ (v "c") (int 1)) (int 1)) (O.v "c");
  (* lsr_int / asr_int (l.231, l.240) *)
  rule "lsr_int:233/zero" I.(lsr_ (v "c") (int 0)) (O.v "c");
  rule "lsr_int:235/const" ~guard:(n > i 0)
    I.(lsr_ (v "c") (ci n)) O.(lsr_ (masked all_but_low_bit (v "c")) (ci n));
  rule "asr_int:242/zero" I.(asr_ (v "c") (int 0)) (O.v "c");
  rule "asr_int:244/const" ~guard:(n > i 0)
    I.(asr_ (v "c") (ci n)) O.(asr_ (masked all_but_low_bit (v "c")) (ci n))

let () =
  let open Host.O in
  let n = cv "n" in
  (* tag_int (l.249) *)
  rule "tag_int:251/const" I.(tag (ci n)) O.(tag (ci n));
  rule "tag_int:253/asr" ~guard:(n > i 0)
    I.(tag (asr_ (v "c") (ci n)))
    O.(or_ (asr_ (v "c") (ci (n - i 1))) (int 1));
  (* untag_int (l.260) *)
  rule "untag_int:262/const" I.(untag (ci n)) O.(ci (n asr i 1));
  rule "untag_int:263/or-asr" ~guard:(n > i 0 && n < i 64)
    I.(untag (or_ (asr_ (v "c") (ci n)) (int 1)))
    O.(asr_ (v "c") (ci (n + i 1)));
  rule "untag_int:266/or-lsr" ~guard:(n > i 0 && n < i 64)
    I.(untag (or_ (lsr_ (v "c") (ci n)) (int 1)))
    O.(lsr_ (v "c") (ci (n + i 1)));
  (* int_const n (l.113) *)
  let max_repr_int = max_int asr i 1 and min_repr_int = min_int asr i 1 in
  rule "int_const:114/small" ~guard:(n <= max_repr_int && n >= min_repr_int)
    I.(tag (ci n)) O.(ci ((n lsl i 1) + i 1));
  rule "int_const:116/large" ~guard:(not (n <= max_repr_int && n >= min_repr_int))
    I.(tag (ci n))
    O.(cn (Binop (Add, Nativeint, Binop (Lsl, Nativeint, Of_int n, Natlit 1L), Natlit 1L)));
  (* natint_const_untagged n (l.118) *)
  let nn = nv "n" in
  rule "natint_const_untagged:122/small"
    ~guard:(Cmp (LE, nn, Of_int max_int) && Cmp (GE, nn, Of_int min_int))
    I.(cn nn) O.(ci (To_int nn))

let () =
  (* mk_if_then_else (l.271) *)
  rule "mk_if_then_else:273/false" I.(ite (int 0) (v "a") (v "b")) (O.v "b");
  rule "mk_if_then_else:274/true" I.(ite (int 1) (v "a") (v "b")) (O.v "a");
  (* mk_not (l.278): Pnot on a tagged boolean (1 or 3) *)
  let not_spec p = I.(sub (int 4) p), [ Pre.Tagged_bool p ] in
  let c = var_cmp "cmp" in
  let check name p out =
    let input, pre = not_spec p in
    rule name ~pre input out
  in
  check "mk_not:283/ccmpi"
    I.(tag (cmp (V c) (v "c1") (v "c2")))
    O.(tag (cmp (Negate (V c)) (v "c1") (v "c2")));
  check "mk_not:286/ccmpa"
    I.(tag (cmpa (V c) (v "c1") (v "c2")))
    O.(tag (cmpa (Negate (V c)) (v "c1") (v "c2")));
  let fc = var_fcmp "fcmp" in
  check "mk_not:289/ccmpf"
    I.(tag (fcmp (V fc) "f1" "f2"))
    O.(tag (fcmp (Negate (V fc)) "f1" "f2"));
  check "mk_not:294/tagged"
    I.(tag (v "c"))
    O.(sub (int 3) (lsl_ (v "c") (int 1)));
  check "mk_not:299/three" I.(int 3) O.(int 1);
  check "mk_not:300/one" I.(int 1) O.(int 3)

let () =
  (* mk_compare_ints (l.305): compare on (tagged or untagged) integers *)
  let spec a b = I.(tag (sub (cmp (K GT) a b) (cmp (K LT) a b))) in
  let c1 = cv "c1" and c2 = cv "c2" and n1 = nv "n1" and n2 = nv "n2" in
  rule "mk_compare_ints:307/int-int"
    (spec I.(ci c1) I.(ci c2)) O.(tag (ci (Compare (Int, c1, c2))));
  rule "mk_compare_ints:309/natint-natint"
    (spec I.(cn n1) I.(cn n2)) O.(tag (ci (Compare (Nativeint, n1, n2))));
  rule "mk_compare_ints:311/int-natint"
    (spec I.(ci c1) I.(cn n2)) O.(tag (ci (Compare (Nativeint, Of_int c1, n2))));
  rule "mk_compare_ints:313/natint-int"
    (spec I.(cn n1) I.(ci c2)) O.(tag (ci (Compare (Nativeint, n1, Of_int c2))));
  (* mk_compare_floats (l.323), against OCaml's [compare] on floats: NaN is
     equal to itself and smaller than any other float. *)
  let is_nan f = I.fcmp (K FNEQ) f f in
  rule "mk_compare_floats:323"
    I.(tag
         (ite (is_nan "a1")
            (ite (is_nan "a2") (int 0) (int (-1)))
            (ite (is_nan "a2") (int 1)
               (sub (fcmp (K FGT) "a1" "a2") (fcmp (K FLT) "a1" "a2")))))
    O.(tag
         (add
            (sub (fcmp (K FGT) "a1" "a2") (fcmp (K FLT) "a1" "a2"))
            (sub (fcmp (K FEQ) "a1" "a1") (fcmp (K FEQ) "a2" "a2"))))

let () =
  let open Host.O in
  (* test_bool (l.568): turns a tagged boolean into a truth value *)
  let n = cv "n" in
  rule "test_bool:570/tagged" ~relation:Truthiness ~pre:[ Tagged_bool I.(tag (v "c")) ]
    I.(tag (v "c")) (O.v "c");
  (* The same, assuming the untagged boolean is canonical *)
  rule "test_bool:570/tagged-canonical" ~relation:Truthiness
    ~pre:[ Tagged_bool I.(tag (v "c")); Is_bool (I.v "c") ]
    I.(tag (v "c")) (O.v "c");
  rule "test_bool:572/const-one" ~relation:Truthiness ~guard:(n = i 1)
    I.(ci n) O.(int 0);
  rule "test_bool:572/const-other" ~relation:Truthiness ~guard:(n <> i 1)
    I.(ci n) O.(int 1)

let () =
  let open Host.O in
  (* div_int c1 c2 (l.440), on untagged integers *)
  let n = cv "n" and n1 = cv "n1" and n2 = cv "n2" in
  let l = log2 n in
  rule "div_int:444/one" I.(div (v "c1") (int 1)) (O.v "c1");
  rule "div_int:446/const-const" ~guard:(n2 <> i 0 && n2 <> i 1)
    I.(div (ci n1) (ci n2)) O.(ci (n1 / n2));
  let not_special = n <> i 0 && n <> i 1 && n <> min_int in
  rule "div_int:448/power2" ~guard:(not_special && M.is_power2 n)
    I.(div (v "c1") (ci n))
    O.(asr_
         (add (v "c1") (lsr_ (asr_ (v "c1") (ci (l - i 1))) (ci (i 64 - l))))
         (ci l));
  rule "div_int:466/negative" ~guard:(not_special && not (M.is_power2 n) && n < i 0)
    I.(div (v "c1") (ci n))
    O.(sub (int 0) (div (v "c1") (ci (neg n))));
  (* Multiply-high by the magic number (m, p) = divimm_parameters n *)
  let m = nv "m" and p = cv "p" in
  List.iter [ true; false ] ~f:(fun m_neg ->
      List.iter [ true; false ] ~f:(fun p_pos ->
          let t = O.(mulhi (v "c1") (cn m)) in
          let t = if m_neg then O.(add t (v "c1")) else t in
          let t = if p_pos then O.(asr_ t (ci p)) else t in
          rule
            (sprintf "div_int:471/magic%s%s"
               (if m_neg then "-m<0" else "-m>=0")
               (if p_pos then "-p>0" else "-p=0"))
            ~guard:(not_special && not (M.is_power2 n) && n > i 0
                    && Divimm { d = Of_int n; m; p }
                    && (if m_neg then Cmp (LT, m, Natlit 0L) else Cmp (GE, m, Natlit 0L))
                    && (if p_pos then p > i 0 else p = i 0))
            I.(div (v "c1") (ci n))
            O.(add t (lsr_ (v "c1") (int 63)))));
  (* mod_int c1 c2 (l.496) *)
  rule "mod_int:500/one" ~guard:(n = i 1 || n = i (-1))
    I.(mod_ (v "c1") (ci n)) O.(seq (v "c1") (int 0));
  rule "mod_int:502/const-const" ~guard:(n2 <> i 0 && n2 <> i 1 && n2 <> i (-1))
    I.(mod_ (ci n1) (ci n2)) O.(ci (n1 mod n2));
  let not_special = not_special && n <> i (-1) in
  rule "mod_int:504/power2" ~guard:(not_special && M.is_power2 n)
    I.(mod_ (v "c1") (ci n))
    O.(sub (v "c1")
         (and_
            (add (v "c1") (lsr_ (asr_ (v "c1") (ci (l - i 1))) (ci (i 64 - l))))
            (ci (neg n))));
  rule "mod_int:521/other" ~guard:(not_special && not (M.is_power2 n))
    I.(mod_ (v "c1") (ci n))
    O.(sub (v "c1") (mul (div (v "c1") (ci n)) (ci n)))

let () =
  let open Host.O in
  (* safe_div_bi / safe_mod_bi (l.544): x / -1 = -x and x mod -1 = 0 for
     boxed integers; the check is omitted when the divisor is known not to be
     -1. Specification: the boxed-integer division, for a non-zero divisor. *)
  let n = cv "n" in
  let div_spec d = I.(ite (cmp (K EQ) d (int (-1))) (sub (int 0) (v "c1")) (div (v "c1") d)) in
  let mod_spec d = I.(ite (cmp (K EQ) d (int (-1))) (int 0) (mod_ (v "c1") d)) in
  rule "safe_div_bi:549/check"
    ~pre:[ Cmp (NE, I.v "c2", I.int 0) ]
    (div_spec (I.v "c2"))
    O.(ite (cmp (K NE) (v "c2") (int (-1))) (div (v "c1") (v "c2")) (sub (int 0) (v "c1")));
  rule "safe_div_bi:549/known-divisor" ~guard:(n <> i 0 && n <> i (-1))
    (div_spec I.(ci n)) O.(div (v "c1") (ci n));
  rule "safe_mod_bi:549/check"
    ~pre:[ Cmp (NE, I.v "c2", I.int 0) ]
    (mod_spec (I.v "c2"))
    O.(ite (cmp (K NE) (v "c2") (int (-1))) (mod_ (v "c1") (v "c2")) (int 0));
  rule "safe_mod_bi:549/known-divisor" ~guard:(n <> i 0 && n <> i (-1))
    (mod_spec I.(ci n)) O.(mod_ (v "c1") (ci n));
  (* For Pint32 the check is omitted: operands are sign-extended 32-bit values *)
  let int32 p =
    [ Pre.Cmp (GE, p, I.int (-0x8000_0000)); Pre.Cmp (LT, p, I.int 0x8000_0000) ]
  in
  rule "safe_div_bi:551/int32"
    ~pre:(Pre.Cmp (NE, I.v "c2", I.int 0) :: int32 (I.v "c1") @ int32 (I.v "c2"))
    (div_spec (I.v "c2")) O.(div (v "c1") (v "c2"));
  rule "safe_mod_bi:551/int32"
    ~pre:(Pre.Cmp (NE, I.v "c2", I.int 0) :: int32 (I.v "c1") @ int32 (I.v "c2"))
    (mod_spec (I.v "c2")) O.(mod_ (v "c1") (v "c2"))

let () =
  (* low_32 (l.1010): agrees with its argument on the low 32 bits *)
  let relation = Relation.Agree_on low_32_bits in
  rule "low_32:1013/sign-extension" ~relation
    I.(asr_ (lsl_ (v "x") (int 32)) (int 32)) O.(masked low_32_bits (v "x"));
  rule "low_32:1015/zero-extension" ~relation
    I.(and_ (v "x") (cn (Natlit 0xFFFFFFFFL))) O.(masked low_32_bits (v "x"));
  (* sign_extend_32 / zero_extend_32 (l.1021, l.1026) *)
  rule "sign_extend_32:1021"
    I.(asr_ (lsl_ (v "e") (int 32)) (int 32))
    O.(asr_ (lsl_ (masked low_32_bits (v "e")) (int 32)) (int 32));
  rule "zero_extend_32:1026"
    I.(and_ (v "e") (cn (Natlit 0xFFFFFFFFL)))
    O.(and_ (masked low_32_bits (v "e")) (cn (Natlit 0xFFFFFFFFL)))

let () =
  (* unaligned_load_{16,32,64} (l.1115-1262) without unaligned access:
     byte loads combined according to endianness *)
  List.iter [ Endian.Little; Big ] ~f:(fun endian ->
      List.iter [ 2, Chunk.Sixteen_unsigned; 4, Thirtytwo_unsigned; 8, Word ]
        ~f:(fun (size, chunk) ->
            let byte k =
              O.(load Byte_unsigned (if k = 0 then add (v "ptr") (v "idx")
                                     else add (add (v "ptr") (v "idx")) (int k)))
            in
            let bytes = List.init size ~f:byte in
            let bytes =
              match endian with Big -> bytes | Little -> List.rev bytes
            in
            let shifted =
              List.mapi bytes ~f:(fun j b ->
                  let shift = 8 * (size - 1 - j) in
                  if shift = 0 then b else O.(lsl_ b (int shift)))
            in
            (* Balanced tree of Cor, as written in the source *)
            let rec combine = function
              | [ x ] -> x
              | l ->
                let left, right = List.split_n l (List.length l / 2) in
                O.or_ (combine left) (combine right)
            in
            rule
              (sprintf "unaligned_load_%i:%s" (8 * size)
                 (match endian with Little -> "little-endian" | Big -> "big-endian"))
              I.(load ~endian chunk (add (v "ptr") (v "idx")))
              (combine shifted)))

let () =
  (* max_or_zero (l.1340) *)
  rule "max_or_zero:1340"
    I.(ite (cmp (K LE) (v "a") (int 0)) (int 0) (v "a"))
    O.(and_ (xor (asr_ (v "a") (int 63)) (int (-1))) (v "a"))

let () =
  let open Host.O in
  (* array_indexing log2size ptr ofs (l.728): address of element [ofs]
     (tagged) of size 2^log2size. Indices are within the maximal array size. *)
  let s = cv "log2size" and n = cv "n" in
  let spec ofs = I.(add (v "ptr") (lsl_ (untag ofs) (ci s))) in
  let pre ofs =
    [ Pre.Tagged ofs
    ; Cmp (GE, I.untag ofs, I.int 0)
    ; Cmp (LT, I.untag ofs, I.int 0x40_0000_0000_0000)
    ]
  in
  let rule ?(guard = true_) name ofs out =
    rule name ~guard:(i 0 <= s && s <= i 3 && guard) ~pre:(pre ofs) (spec ofs) out
  in
  rule "array_indexing:735/const-zero" ~guard:((n asr i 1) = i 0) I.(ci n) (O.v "ptr");
  rule "array_indexing:737/const" ~guard:((n asr i 1) <> i 0)
    I.(ci n) O.(add (v "ptr") (ci ((n asr i 1) lsl s)));
  rule "array_indexing:739/tagged" I.(tag (v "c")) O.(add (v "ptr") (lsl_ (v "c") (ci s)));
  rule "array_indexing:742/add-const-byte" ~guard:(s = i 0)
    I.(add (v "c") (ci n))
    O.(add (add (v "ptr") (untag (v "c"))) (ci (n asr i 1)));
  rule "array_indexing:746/add-const" ~guard:(s > i 0)
    I.(add (v "c") (ci n))
    O.(add (add (v "ptr") (lsl_ (v "c") (ci (s - i 1)))) (ci ((n - i 1) lsl (s - i 1))));
  rule "array_indexing:751/other" ~guard:(s > i 0)
    (I.v "ofs")
    O.(add (add (v "ptr") (lsl_ (v "ofs") (ci (s - i 1)))) (ci (i (-1) lsl (s - i 1))))

let () =
  let open Host.O in
  (* Operations on OCaml integers (l.1415-1490): arguments are tagged *)
  let pre = [ Pre.Tagged (I.v "a"); Tagged (I.v "b") ] in
  let a = I.v "a" and b = I.v "b" in
  rule "add_int_caml:1415" ~pre
    I.(tag (add (untag a) (untag b))) O.(add (add (v "a") (v "b")) (int (-1)));
  rule "sub_int_caml:1425" ~pre
    I.(tag (sub (untag a) (untag b))) O.(add (sub (v "a") (v "b")) (int 1));
  let k = cv "k" in
  rule "mul_int_caml:1436/const" ~pre:[ Tagged I.(ci k); Tagged b ]
    I.(tag (mul (untag (ci k)) (untag b)))
    O.(add (mul (untag (ci k)) (add (v "b") (int (-1)))) (int 1));
  rule "mul_int_caml:1438" ~pre
    I.(tag (mul (untag a) (untag b)))
    O.(add (mul (add (v "a") (int (-1))) (untag (v "b"))) (int 1));
  rule "and_int_caml:1450" ~pre
    I.(tag (and_ (untag a) (untag b))) O.(and_ (v "a") (v "b"));
  rule "or_int_caml:1453" ~pre
    I.(tag (or_ (untag a) (untag b))) O.(or_ (v "a") (v "b"));
  rule "xor_int_caml:1456" ~pre
    I.(tag (xor (untag a) (untag b)))
    O.(or_ (xor (masked all_but_low_bit (v "a")) (masked all_but_low_bit (v "b"))) (int 1));
  (* Shifts by n are unspecified for n < 0 or n > Sys.int_size *)
  let shift_pre =
    pre @ [ Cmp (GE, I.untag b, I.int 0); Cmp (LE, I.untag b, I.int 63) ]
  in
  rule "lsl_int_caml:1461" ~pre:shift_pre
    I.(tag (lsl_ (untag a) (untag b)))
    O.(add (lsl_ (add (v "a") (int (-1))) (untag (v "b"))) (int 1));
  rule "lsr_int_caml:1465" ~pre:shift_pre
    I.(tag (lsr_ (and_ (untag a) (cn all_but_high_bit)) (untag b)))
    O.(or_ (lsr_ (v "a") (untag (v "b"))) (int 1));
  rule "asr_int_caml:1469" ~pre:shift_pre
    I.(tag (asr_ (untag a) (untag b)))
    O.(or_ (asr_ (v "a") (untag (v "b"))) (int 1));
  let c = var_cmp "cmp" in
  rule "int_comp_caml:1473" ~pre
    I.(tag (cmp (V c) (untag a) (untag b))) O.(tag (cmp (V c) (v "a") (v "b")));
  rule "negint:2213" ~pre:[ Tagged a ]
    I.(tag (sub (int 0) (untag a))) O.(sub (int 2) (v "a"));
  let n = cv "n" in
  rule "offsetint:1420/small" ~pre:[ Tagged a ] ~guard:(M.no_overflow_lsl n (i 1))
    I.(tag (add (untag a) (ci n))) O.(add (v "a") (ci (n lsl i 1)));
  rule "offsetint:1422/large" ~pre:[ Tagged a ] ~guard:(not (M.no_overflow_lsl n (i 1)))
    I.(tag (add (untag a) (ci n))) O.(tag (add (untag (v "a")) (untag (tag (ci n)))));
  (* Pisout h arg: arg outside [0, h] *)
  rule "transl_isout:1411"
    ~pre:[ Tagged (I.v "h"); Tagged (I.v "arg"); Cmp (GE, I.(untag (v "h")), I.int 0) ]
    I.(tag (or_ (cmp (K LT) (untag (v "arg")) (int 0))
              (cmp (K GT) (untag (v "arg")) (untag (v "h")))))
    O.(tag (cmpa (K LT) (v "h") (v "arg")))

let () =
  (* cmmgen.ml l.601: (compare a b) cmp 0 ~> a cmp b, for tagged [int]s
     ([Pcompare_ints]) and for unboxed integers ([Pcompare_bints]) *)
  let c = var_cmp "cmp" in
  rule "cmmgen:601/compare-against-zero"
    I.(tag (cmp (V c)
              (tag (sub (cmp (K GT) (v "a") (v "b")) (cmp (K LT) (v "a") (v "b"))))
              (int 1)))
    O.(tag (cmp (V c) (v "a") (v "b")))

let all = Queue.to_list rules

(* Rules whose symbolic form is too hard for the solver (division and
   multiplication by a symbolic constant), instantiated with concrete
   constants. The division parameters are those computed by the compiler's
   own [divimm_parameters] (see [Divimm]). *)
let instances ?(divisors = []) () =
  let find name = List.Assoc.find_exn all name ~equal:String.equal in
  let inst name values =
    let suffix =
      List.map values ~f:(fun (var, value) -> sprintf "%s=%Ld" var value)
      |> String.concat ~sep:","
    in
    name ^ "@" ^ suffix, Rule.instantiate (find name) ~values
  in
  let is_power2 d = d > 0 && d land (d - 1) = 0 in
  let powers = List.map (List.range 1 62) ~f:(fun l -> 1 lsl l) in
  let divisors =
    if not (List.is_empty divisors) then divisors
    else
      List.filter (List.range 3 64) ~f:(fun d -> not (is_power2 d))
      @ [ 100; 641; 1000; 1_000_000_007; 0x7FFF_FFFF; 0x1_0000_0001
        ; 0x3FFF_FFFF_FFFF_FFFF (* max_int *); 0x3FFF_FFFF_FFFF_FFFD
        ; 0x2AAA_AAAA_AAAA_AAAB; (1 lsl 61) + 1; (1 lsl 61) - 1 ]
  in
  let n v = [ "n", Int64.of_int v ] in
  List.concat
    [ List.map powers ~f:(fun d -> inst "div_int:448/power2" (n d))
    ; List.map powers ~f:(fun d -> inst "mod_int:504/power2" (n d))
    ; List.map divisors ~f:(fun d ->
          let m, p = Divimm.divimm_parameters d in
          let variant =
            sprintf "div_int:471/magic%s%s"
              (if Int64.(m < 0L) then "-m<0" else "-m>=0")
              (if p > 0 then "-p>0" else "-p=0")
          in
          inst variant
            [ "n", Int64.of_int d; "m", m; "p", Int64.of_int p ])
    ; List.map (divisors @ powers) ~f:(fun d -> inst "div_int:466/negative" (n (-d)))
    ; List.map (divisors @ List.map divisors ~f:Int.neg @ List.map powers ~f:Int.neg)
        ~f:(fun d -> inst "mod_int:521/other" (n d))
    ; List.concat_map
        ([ -10; -9; -7; -6; -5; -3; -2; 3; 5; 6; 7; 9; 10; 1000; -1000; 0x7FFF_FFFF
         ; Int.max_value; Int.min_value + 1 ])
        ~f:(fun k ->
            [ inst "mul_int:209/add-const-right" [ "k", Int64.of_int k ]
            ; inst "mul_int:210/add-const-left" [ "k", Int64.of_int k ] ])
    ]
