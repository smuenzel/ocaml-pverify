open! Core

let defines =
  [ "BITS", "64"
  ; "HIGHBIT2", "62"
  ; "HIGHBIT", "63"
  ; "ZERO", "#x0000000000000000"
  ; "ONE", "#x0000000000000001"
  ; "NEGONE", "#xffffffffffffffff"
  ; "MININT", "#x8000000000000000"
  ; "WORDBITS", "#x0000000000000040"
  ; "HOSTBITS", "#x000000000000003f"
  ]

let preamble = {|

(define-sort ocaml-int () (_ BitVec BITS))

(declare-datatypes () ((CMP EQ NE LT GT LE GE)))

(declare-datatypes () ((ILOG AND OR XOR)))

(define-fun ocaml-lsl ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvshl x y))

(define-fun ocaml-lsr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvlshr x y))

(define-fun ocaml-or ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvor x y))

(define-fun ocaml-and ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvand x y))

(define-fun ocaml-xor ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvxor x y))

(define-fun ocaml-asr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvashr x y))

(define-fun ocaml-has-tag ((x ocaml-int)) Bool
  (= (ocaml-and x ONE) ONE))

(define-fun ocaml-highest-bits-same ((x ocaml-int)) Bool
  (= ((_ extract HIGHBIT HIGHBIT) x) ((_ extract HIGHBIT2 HIGHBIT2) x)))

(define-fun ocaml-lsr-1 ((x ocaml-int)) ocaml-int
  (ocaml-lsr x ONE))

(define-fun ocaml-addi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvadd x y))

(define-fun ocaml-subi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsub x y))

(define-fun ocaml-muli ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvmul x y))

(define-fun ocaml-mulhi ((x ocaml-int) (y ocaml-int)) ocaml-int
  ((_ extract 127 64)
   (bvmul ((_ sign_extend 64) x) ((_ sign_extend 64) y))))

(define-fun ocaml-divi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsdiv x y))

(define-fun ocaml-modi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsrem x y))

(define-fun ocaml-tagi ((x ocaml-int)) ocaml-int
  (bvadd (ocaml-lsl x ONE) ONE))

(define-fun ocaml-untagi ((x ocaml-int)) ocaml-int
  (ocaml-asr x ONE))

(define-fun ocaml-cmp-eq ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (= x y) ONE ZERO))

(define-fun ocaml-cmp-ne ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (not (= x y)) ONE ZERO))

(define-fun ocaml-cmp-lt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvslt x y)
    ONE ZERO))

(define-fun ocaml-cmp-gt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsgt x y)
    ONE ZERO))

(define-fun ocaml-cmp-le ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsle x y)
    ONE ZERO))

(define-fun ocaml-cmp-ge ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (bvsge x y)
    ONE ZERO))

(define-fun ocaml-cmp ((cmp CMP) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match cmp
    (case EQ (ocaml-cmp-eq x y))
    (case NE (ocaml-cmp-ne x y))
    (case LT (ocaml-cmp-lt x y))
    (case GT (ocaml-cmp-gt x y))
    (case LE (ocaml-cmp-le x y))
    (case GE (ocaml-cmp-ge x y))
    ))

; Ccmpa: unsigned comparison
(define-fun ocaml-cmpa ((cmp CMP) (x ocaml-int) (y ocaml-int)) ocaml-int
  (ite
    (ite (= cmp EQ) (= x y)
    (ite (= cmp NE) (not (= x y))
    (ite (= cmp LT) (bvult x y)
    (ite (= cmp GT) (bvugt x y)
    (ite (= cmp LE) (bvule x y)
    (bvuge x y))))))
    ONE ZERO))

; Lambda.negate_integer_comparison / swap_integer_comparison
; (tables extracted from the installed compiler-libs)
(define-fun cmp-negate ((c CMP)) CMP
  (ite (= c EQ) NE (ite (= c NE) EQ (ite (= c LT) GE
  (ite (= c GT) LE (ite (= c LE) GT LT))))))

(define-fun cmp-swap ((c CMP)) CMP
  (ite (= c EQ) EQ (ite (= c NE) NE (ite (= c LT) GT
  (ite (= c GT) LT (ite (= c LE) GE LE))))))

(define-fun ocaml-ilog ((ilog ILOG) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match ilog
    (case AND (ocaml-and x y))
    (case OR  (ocaml-or x y))
    (case XOR (ocaml-xor x y))
  ))

; Floats

(define-sort ocaml-float () (_ FloatingPoint 11 53))

(declare-datatypes () ((FCMP FEQ FNEQ FLT FNLT FGT FNGT FLE FNLE FGE FNGE)))

(define-fun ocaml-cmpf ((c FCMP) (x ocaml-float) (y ocaml-float)) ocaml-int
  (ite
    (ite (= c FEQ) (fp.eq x y)
    (ite (= c FNEQ) (not (fp.eq x y))
    (ite (= c FLT) (fp.lt x y)
    (ite (= c FNLT) (not (fp.lt x y))
    (ite (= c FGT) (fp.gt x y)
    (ite (= c FNGT) (not (fp.gt x y))
    (ite (= c FLE) (fp.leq x y)
    (ite (= c FNLE) (not (fp.leq x y))
    (ite (= c FGE) (fp.geq x y)
    (not (fp.geq x y)))))))))))
    ONE ZERO))

; Lambda.negate_float_comparison / swap_float_comparison
(define-fun fcmp-negate ((c FCMP)) FCMP
  (ite (= c FEQ) FNEQ (ite (= c FNEQ) FEQ (ite (= c FLT) FNLT
  (ite (= c FNLT) FLT (ite (= c FGT) FNGT (ite (= c FNGT) FGT
  (ite (= c FLE) FNLE (ite (= c FNLE) FLE (ite (= c FGE) FNGE FGE))))))))))

(define-fun fcmp-swap ((c FCMP)) FCMP
  (ite (= c FEQ) FEQ (ite (= c FNEQ) FNEQ (ite (= c FLT) FGT
  (ite (= c FNLT) FNGT (ite (= c FGT) FLT (ite (= c FNGT) FNLT
  (ite (= c FLE) FGE (ite (= c FNLE) FNGE (ite (= c FGE) FLE FNLE))))))))))

; Host arithmetic. Host [int]s are 63 bits, kept sign-extended to 64 bits.

(define-fun host-wrap ((x ocaml-int)) ocaml-int
  ((_ sign_extend 1) ((_ extract HIGHBIT2 0) x)))

(define-fun host-lsr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvlshr (bvand x (bvlshr NEGONE ONE)) y))

(define-fun host-compare ((x ocaml-int) (y ocaml-int)) ocaml-int
  (ite (bvslt x y) NEGONE (ite (= x y) ZERO ONE)))

(define-fun host-ucompare ((x ocaml-int) (y ocaml-int)) ocaml-int
  (ite (bvult x y) NEGONE (ite (= x y) ZERO ONE)))

; Characterisation of [(m, p) = Cmm_helpers.divimm_parameters d] given in
; cmm_helpers.ml:  2^(64 + p) < m * d <= 2^(64 + p) + 2^(p + 1)
; with [m] unsigned, and [d > 0], [0 <= p < 64].
(define-fun divimm-ok ((d ocaml-int) (m ocaml-int) (p ocaml-int)) Bool
  (let ((md (bvmul ((_ zero_extend 128) m) ((_ zero_extend 128) d)))
        (p192 ((_ zero_extend 128) p))
        (one192 ((_ zero_extend 191) #b1)))
  (let ((lo (bvshl one192 (bvadd p192 ((_ zero_extend 128) WORDBITS)))))
  (and (bvsgt d ZERO)
       (bvsge p ZERO)
       (bvslt p WORDBITS)
       (bvult lo md)
       (bvule md (bvadd lo (bvshl one192 (bvadd p192 ((_ zero_extend 191) #b1)))))))))

; Memory: loads read bytes from an array

(define-sort ocaml-mem () (Array ocaml-int (_ BitVec 8)))

(define-fun mem-byte ((m ocaml-mem) (a ocaml-int) (i ocaml-int)) (_ BitVec 8)
  (select m (bvadd a i)))

  |}

(* [Misc.log2]: 0 for [n <= 1], otherwise the index of the highest set bit *)
let host_log2 =
  let body =
    List.fold (List.range 1 63) ~init:"#x0000000000000000"
      ~f:(fun acc bit ->
          sprintf "(ite (= ((_ extract %i %i) n) #b1) %s %s)"
            bit bit (Smt.Smt_int.of_int bit) acc)
  in
  sprintf
    "(define-fun host-log2 ((n ocaml-int)) ocaml-int\n  (ite (bvsle n ONE) ZERO %s))\n"
    body

let loads =
  let bytes ~endian n =
    let idx = List.init n ~f:Fn.id in
    (* concat takes the most significant byte first *)
    let order =
      match (endian : Spec.Endian.t) with
      | Little -> List.rev idx
      | Big -> idx
    in
    List.map order ~f:(fun i -> sprintf "(mem-byte m a %s)" (Smt.Smt_int.of_int i))
  in
  let concat l =
    match l with
    | [ x ] -> x
    | l -> sprintf "(concat %s)" (String.concat ~sep:" " l)
  in
  List.concat_map [ Spec.Endian.Little; Big ] ~f:(fun endian ->
      let e = match endian with Little -> "le" | Big -> "be" in
      List.map [ 1; 2; 4; 8 ] ~f:(fun n ->
          let raw = concat (bytes ~endian n) in
          let ext kind =
            if n = 8 then raw
            else sprintf "((_ %s_extend %i) %s)" kind (64 - 8 * n) raw
          in
          sprintf
            "(define-fun load-u%i-%s ((m ocaml-mem) (a ocaml-int)) ocaml-int %s)\n\
             (define-fun load-s%i-%s ((m ocaml-mem) (a ocaml-int)) ocaml-int %s)\n"
            (8 * n) e (ext "zero") (8 * n) e (ext "sign")
        ))
  |> String.concat

let full_preamble : string =
  (* Longest names first, so that e.g. NEGONE is not clobbered by ONE *)
  List.fold ~init:(preamble ^ host_log2 ^ "\n" ^ loads)
    (List.sort defines ~compare:(fun (a, _) (b, _) ->
         Int.compare (String.length b) (String.length a)))
    ~f:(fun acc (pattern, with_) ->
        String.substr_replace_all ~pattern ~with_ acc
      )
