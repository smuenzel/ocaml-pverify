
(define-sort ocaml-int () (_ BitVec 5))

(declare-datatypes () ((CMP EQ NE LT GT LE GE)))

(define-fun ocaml-lsl ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvshl x y))

(define-fun ocaml-lsr ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvlshr x y))

(define-fun ocaml-or ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvor x y))

(define-fun ocaml-and ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvand x y))

(define-fun ocaml-has-tag ((x ocaml-int)) bool
  (= (ocaml-and x #b00001) #b00001))

(define-fun ocaml-highest-bits-same ((x ocaml-int)) bool
  (= ((_ extract 4 4) x) ((_ extract 3 3) x)))

(define-fun ocaml-asr-1 ((x ocaml-int)) ocaml-int
  (concat
    ((_ extract 4 4) x)
    ((_ extract 3 0) (ocaml-lsr x #b00001))
    ))

(define-fun ocaml-lsr-1 ((x ocaml-int)) ocaml-int
  (ocaml-lsr x #b00001))

(define-fun ocaml-addi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvadd x y))

(define-fun ocaml-subi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvsub x y))

(define-fun ocaml-tagi ((x ocaml-int)) ocaml-int
  (bvadd (ocaml-lsl x #b00001) #b00001))

(define-fun ocaml-untagi ((x ocaml-int)) ocaml-int
  (ocaml-asr-1 x))

(define-fun ocaml-to-int-untagged ((x ocaml-int)) Int
  (+ (if (= ((_ extract 4 4) x) #b1) -16 0)
  (bv2int ((_ extract 3 0) x))))

(define-fun ocaml-cmp-eq ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (= x y) #b00001 #b00000))

(define-fun ocaml-cmp-ne ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (not (= x y)) #b00001 #b00000))

(define-fun ocaml-cmp-lt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (< (ocaml-to-int-untagged x) (ocaml-to-int-untagged y))
    #b00001 #b00000))

(define-fun ocaml-cmp-gt ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (> (ocaml-to-int-untagged x) (ocaml-to-int-untagged y))
    #b00001 #b00000))

(define-fun ocaml-cmp-le ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (<= (ocaml-to-int-untagged x) (ocaml-to-int-untagged y))
    #b00001 #b00000))

(define-fun ocaml-cmp-ge ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (>= (ocaml-to-int-untagged x) (ocaml-to-int-untagged y))
    #b00001 #b00000))

(define-fun ocaml-cmp ((cmp CMP) (x ocaml-int) (y ocaml-int)) ocaml-int
  (match cmp
    (case EQ (ocaml-cmp-eq x y))
    (case NE (ocaml-cmp-ne x y))
    (case LT (ocaml-cmp-lt x y))
    (case GT (ocaml-cmp-gt x y))
    (case LE (ocaml-cmp-le x y))
    (case GE (ocaml-cmp-ge x y))
    ))
  

;| Cop(Ccmpi cmpop
;￼	       , [ Cop(Caddi,[Cop(Clsl,[left;Cconst_int(1,_)],_);Cconst_int(1,_)],_)
;￼	         ; Cconst_int (right_i,right_dbg)
;￼	         ]
;￼	       , dbg_cmp
;￼	       ) ->
;￼	      Cop (Ccmpi cmpop, [ left; Cconst_int (right_i asr 1, right_dbg)], dbg_cmp)

(declare-const left ocaml-int)
(declare-const right ocaml-int)
(declare-const cmpop CMP)

(push 1)

(assert (not (=
  (ocaml-cmp-eq (ocaml-tagi left) right)
  (ocaml-cmp-eq left (ocaml-asr-1 right))
  )))

(assert (ocaml-has-tag right))
(assert (ocaml-highest-bits-same left))
(check-sat)
(get-model)

(pop 1)

(push 1)

(assert (not (=
  (ocaml-cmp cmpop (ocaml-tagi left) right)
  (ocaml-cmp cmpop (ocaml-lsl left #b00001) (ocaml-subi right #b00001))
  )))

(assert (ocaml-has-tag right))

(check-sat)
(get-model)

(pop 1)
