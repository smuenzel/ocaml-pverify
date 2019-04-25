open! Core

let defines =
  [ "BITS", "64"
  ; "HIGHBIT", "63"
  ; "HIGHBIT2", "62"
  ; "ZERO", "#x0000000000000000"
  ; "ONE", "#x0000000000000001"
  ; "NEGONE", "#x1111111111111111"
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

(define-fun ocaml-has-tag ((x ocaml-int)) bool
  (= (ocaml-and x ONE) ONE))

(define-fun ocaml-highest-bits-same ((x ocaml-int)) bool
  (= ((_ extract HIGHBIT HIGHBIT) x) ((_ extract HIGHBIT2 HIGHBIT2) x)))

(define-fun ocaml-lsr-1 ((x ocaml-int)) ocaml-int
  (ocaml-lsr x ONE))

(define-fun ocaml-addi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvadd x y))

(define-fun ocaml-subi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsub x y))

(define-fun ocaml-tagi ((x ocaml-int)) ocaml-int
  (bvadd (ocaml-lsl x ONE) ONE))

(define-fun ocaml-untagi ((x ocaml-int)) ocaml-int
  (ocaml-asr x ONE))

(define-fun ocaml-to-int-untagged ((x ocaml-int)) Int
  (+ (if (= ((_ extract HIGHBIT HIGHBIT) x) #b1) -16 0)
  (bv2int ((_ extract HIGHBIT2 0) x))))

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

(define-fun ocaml-ilog ((ilog ILOG) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match ilog
    (case AND (ocaml-and x y))
    (case OR  (ocaml-or x y))
    (case XOR (ocaml-xor x y))
  ))

  |}


let full_preamble =
  List.fold ~init:preamble
    ~f:(fun acc (pattern, with_) ->
        String.substr_replace_all ~pattern ~with_ acc
      )
