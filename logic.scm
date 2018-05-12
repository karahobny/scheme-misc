(define true  #t)
(define ⊤     #t) ; verum (down tack) (U22A4)
(define false #f)
(define ⊥     #f) ; falsum (up tack) (U22A5)

(define-syntax-rule (/\ . x) (and . x))
(define-syntax-rule (∧  . x) (and . x)) ; logical'and (U2227)
(define-syntax-rule (⋀  . x) (and . x)) ; n-ary logical 'and' (U22C0)
(define-syntax-rule (\/ . x) (or  . x))
(define-syntax-rule (∨  . x) (or  . x)) ; logical 'or' (U2228)
(define-syntax-rule (⋁  . x) (or  . x)) ; n-ary logical 'or' (U22C1)
(define-syntax-rule (¬  . x) (not . x)) ; unicode logical symbol 'not' (U00AC)
(define-syntax-rule (~  . x) (not . x))

(define-syntax-rule (/= . x) (not (= . x)))
(define-syntax-rule (/> . x) (not (> . x)))
(define-syntax-rule (/< . x) (not (< . x)))

(define (every*? p xs)
  (ε rec α (cond ((Ø? xs)     ⊤)
                 ((p (hd xs)) (α (tl xs)))
                 (else        ⊥))
     in (α p xs)))

(define (≬ x y z)
  (⋀ (< x y) (< y z)))

(define between? ≬)

(define-syntax ⊼
  (syntax-rules ()
    ((_ e)        (if (¬ e) ⊤ ⊥))
    ((_ e e* ...) (if (¬ e) ⊥ (⊼ e* ...)))))

(define-syntax ⊽
  (syntax-rules ()
    ((_ e)        (if e ⊥ ⊤))
    ((_ e e* ...) (if e ⊥ (⊽ e* ...)))))

(:=/stx (nand . x) (⊼ . x))
(:=/stx (nor  . x) (⊽ . x))
