
(define-sort ocaml-int () (_ BitVec 5))

(define-fun ocaml-shl ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvshl x y))

(define-fun ocaml-shr ((x ocaml-int) (y ocaml-int)) ocaml-int
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
    ((_ extract 3 0) (ocaml-shr x #b00001))
    ))

(define-fun ocaml-addi ((x ocaml-int) (y ocaml-int)) ocaml-int
  (bvadd x y))

(define-fun ocaml-tagi ((x ocaml-int)) ocaml-int
  (bvadd (ocaml-shl x #b00001) #b00001))

(define-fun ocaml-untagi ((x ocaml-int)) ocaml-int
  (ocaml-asr-1 x))

(define-fun ocaml-cmp-eq ((x ocaml-int) (y ocaml-int)) ocaml-int
  (if (= x y) #b00001 #b00000))

(define-fun ocaml-to-int-untagged ((x ocaml-int)) Int
  (+ (if (= ((_ extract 4 4) x) #b1) -16 0)
  (bv2int ((_ extract 3 0) x))))

;| Cop(Ccmpi cmpop
;￼	       , [ Cop(Caddi,[Cop(Clsl,[left;Cconst_int(1,_)],_);Cconst_int(1,_)],_)
;￼	         ; Cconst_int (right_i,right_dbg)
;￼	         ]
;￼	       , dbg_cmp
;￼	       ) ->
;￼	      Cop (Ccmpi cmpop, [ left; Cconst_int (right_i asr 1, right_dbg)], dbg_cmp)

(declare-const left ocaml-int)
(declare-const right ocaml-int)

(assert (ocaml-has-tag right))
(assert (ocaml-highest-bits-same left))
(assert (not (=
  (ocaml-cmp-eq (ocaml-tagi left) right)
  (ocaml-cmp-eq left (ocaml-asr-1 right))
  )))
(check-sat)
(get-model)
