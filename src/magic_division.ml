open! Core

(* [div_int c1 (Cconst_int d)] for d > 0 not a power of 2 (cmm_helpers.ml
   l.471) emits, with (m, p) = divimm_parameters d:

     t = Cmulhi(c1, m)
     if m < 0 then t = t + c1
     if p > 0 then t = t asr p
     res = t + (c1 lsr 63)

   Bit-blasting this against a 64-bit divider does not terminate in
   reasonable time, so this states it in linear integer arithmetic, with the
   exact integer meaning of each Cmm operation on 64-bit words:
   - Cmulhi(x, m) = floor(x * m / 2^64)            (signed high product)
   - x + y        = x + y - k * 2^64 for the k that lands in [-2^63, 2^63)
   - x asr p      = floor(x / 2^p)
   - x lsr 63     = 1 if x < 0 else 0
   and checks that [res] is the truncated quotient for every 64-bit c1. *)

let pow2 n = Z.shift_left Z.one n
let word = pow2 64
let half = pow2 63

let int b =
  if Z.lt b Z.zero
  then sprintf "(- %s)" (Z.to_string (Z.neg b))
  else Z.to_string b

(* [m] is the nativeint the compiler emits, as a signed integer *)
let smt ~name ~d ~m ~p =
  let d = Z.of_int d in
  let buf = Buffer.create 1024 in
  let pr fmt = ksprintf (fun s -> Buffer.add_string buf s; Buffer.add_char buf '\n') fmt in
  let in_word x = sprintf "(and (<= %s %s) (< %s %s))" (int (Z.neg half)) x x (int half) in
  pr "(push 1)";
  pr "(echo \"verifying %s\")" name;
  pr "(declare-const c1 Int)";
  pr "(declare-const t0 Int)";
  pr "(declare-const k1 Int)";
  pr "(declare-const t1 Int)";
  pr "(declare-const t2 Int)";
  pr "(declare-const k3 Int)";
  pr "(declare-const res Int)";
  let constraints =
    [ in_word "c1"
    (* t0 = Cmulhi(c1, m) *)
    ; sprintf "(<= 0 (- (* %s c1) (* %s t0)))" (int m) (int word)
    ; sprintf "(< (- (* %s c1) (* %s t0)) %s)" (int m) (int word) (int word)
    (* t1 = t0 + c1 (wrapping) if m < 0 *)
    ; (if Z.lt m Z.zero
       then sprintf "(and (= t1 (- (+ t0 c1) (* k1 %s))) %s)" (int word) (in_word "t1")
       else "(and (= t1 t0) (= k1 0))")
    (* t2 = t1 asr p *)
    ; sprintf "(<= 0 (- t1 (* %s t2)))" (int (pow2 p))
    ; sprintf "(< (- t1 (* %s t2)) %s)" (int (pow2 p)) (int (pow2 p))
    (* res = t2 + (c1 lsr 63) (wrapping) *)
    ; sprintf "(= res (- (+ t2 (ite (< c1 0) 1 0)) (* k3 %s)))" (int word)
    ; in_word "res"
    ]
  in
  let assumptions = sprintf "(assert (and %s))" (String.concat ~sep:" " constraints) in
  (* same protocol as the rule files: first check that the assumptions are
     satisfiable, then the claim *)
  pr "(push 1)";
  pr "(echo \"assumptions satisfiable?\")";
  pr "%s" assumptions;
  pr "(check-sat)";
  pr "(pop 1)";
  pr "%s" assumptions;
  (* res = c1 / d, truncated towards zero *)
  pr "(assert (not (ite (>= c1 0) (and (<= 0 (- c1 (* %s res))) (< (- c1 (* %s res)) %s)) \
      (and (< (- %s) (- c1 (* %s res))) (<= (- c1 (* %s res)) 0)))))"
    (int d) (int d) (int d) (int d) (int d) (int d);
  pr "(check-sat)";
  pr "(get-value (c1 res))";
  pr "(pop 1)";
  Buffer.contents buf

(* One file per divisor; [controls] adds files with a perturbed magic number,
   which must produce counterexamples. *)
let write_dir ~dirname ~divisors ~controls ~first_index =
  List.iteri divisors ~f:(fun index d ->
      let index = first_index + index in
      let m, p = Divimm.divimm_parameters d in
      let m = Z.of_int64 m in
      let write suffix name m =
        Out_channel.write_all
          (dirname ^/ sprintf "%04i_magic_%i%s.smt2" index d suffix)
          ~data:(smt ~name ~d ~m ~p)
      in
      write "" (sprintf "div_int:471/magic@n=%i (m=%s,p=%i)" d (Z.to_string m) p) m;
      if controls
      then
        List.iter [ 1; -1 ] ~f:(fun delta ->
            write (sprintf "_control%+d" delta)
              (sprintf "CONTROL m%+d, expect counterexample: div_int:471/magic@n=%i" delta d)
              (Z.add m (Z.of_int delta))))
