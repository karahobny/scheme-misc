;; -*- geiser-scheme-implementation: guile -*-

;; had some nice help at the folks from r/scheme. Thanks to you all.

;; TODO: modulize these when you have the time

;; general abbreviations on macro patterns to match are:
;; x, y, z, a, b, c => single variables or values. a can also stand for args.
;; v ...             => multiple variables or values
;; e (...)           => expression(s)
;; (.) r            => rest of the pattern

;; asterisks are there only to denote multiple of the
;; corresponding pattern.

;; guix monads library
;; TODO: applying this to some IO and stateful functions
(add-to-load-path "/home/jamppa/src/scheme/")
(add-to-load-path "/home/jamppa/src/scheme/guile-monads")
;; (primitive-load-path "monads.scm")

(use-modules (ice-9  match)
             (ice-9  format)
             (ice-9  control)
             (ice-9  curried-definitions)
             (srfi   srfi-1)
             (srfi   srfi-9)
             (srfi   srfi-26)
             (srfi   srfi-41)
             (monads)
             (monads io)
             (oop    goops))

(read-enable  'r7rs-symbols)
(print-enable 'r7rs-symbols)

;; quick guide to abbreviatons:
;; / <- procedure to be applied on list (from left side).
;;      generally standing for a list, since they are evaluated
;;      from the left by default.
;; \ <- procedure to be applied on a list (from right side)
;;      (also works as a lambda-operator ala Haskell)
;; $> <- apply operator to the following
;; ^> <- compose operator to the preceding (ie. /^> := foldl).
;; @  <- reference to the nth element
;;       (ie. O@, V@ := O-ref and V-ref respectively).
;; $^> <- compose from the applied procedures
;;        (ie. $>^>'', compose from two lists and apply proc to the first
;;         element of each one of them to each other.
;; '   <- quote works in a three-fold way. a derivative of a proc,
;;        identifying something as a list (like generally `quote' works as)
;;        or as a counter for the number of lists the function is applied to.

;; usage of these might be contradictory atm but i wish to adjust them properly.

;;;; *** lambda ***
;; either \ or λ to stand for enhanced lambda.
;; `=>' to separate variables from the expression.
;; without variables and `=>', uses a single variable
;; accesed with an underscore, `_'.

;; lambda rest args works with two dots after arg now.
;; y-combinator shown as an example.

;; with (λ . r) pattern matching you can simply
;; resort to basic lambda notation in those cases.

;; Example:
;;  ((λ x y => (+ x y)) 5 10) => 15
;;  ((λ (+ _ 10)) 5)          => 15
(define-syntax =>
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'=>)))

(define-syntax ..
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'..)))

(define-syntax λ
  (lambda (stx)
    (syntax-case stx(=> ..)
      ((λ (e ...))
       (with-syntax ((x (datum->syntax #'λ '_))) #'(lambda (x) (e ...))))
      ((_ v ..  => (e ...)) #'(lambda v (e ...)))
      ((_ v ... => (e ...)) #'(lambda (v ...) (e ...)))
      ((_ x y   =>  e ...)  #'(lambda (x y) e ...))
      ((_ x     =>  e ...)  #'(lambda (x) e ...))
      ((_       =>  e ...)  #'(lambda () e ...))
      ((_ . r)              #'(lambda . r)))))

(define-syntax lambda^
  (lambda (stx)
    (syntax-case stx ()
      ((_ v e e* ...)
       #'(λ v =>
            (call/cc
             (λ escape =>
                (syntax-parameterize
                    ((return
                      (syntax-rules ()
                        ((return vals (... ...))
                         (escape vals (... ...))))))
                  e e* ...))))))))

(define-syntax-rule (λ^ . x) (lambda^ . x))


;;;; *** primitives ***
;;; define / define-syntax-rule
;; fast assignment for corner-cases where
;; you want terse code fast, one-liners etc.
;;  not to be really used.
(define-syntax-rule (:=     . x) (define             . x))
(define-syntax-rule (:=/stx . x) (define-syntax-rule . x))

;;; cons
(define-syntax-rule (::  . x) (cons  . x))
(define-syntax-rule (::* . x) (cons* . x))

;;; apply / compose
;; $> for general applying procs, and ^> for
;; composing procs.
(define $> apply)
(define ^> compose)

;;; cxr
;; car
(define hd   car)
(define head car)
;; cdr
(define tl   cdr)
(define tail cdr)
;; rest
(load "cxr.scm")

;;; lists / vectors
(define null '())
(define Ø    '())
(define O    list)
(define V    vector)

(:= O^> make-list)
(:= V^> make-vector)


;;;; *** let ***

;; Example:
;;         (let^ α 10 β 20 γ (+ α β) in γ)
;; Use linebreaks atm to differentiate between the vars
;; and their binds from each other.

(define-syntax rec
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'rec)))

(define-syntax in
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'in)))

(define-syntax let^
  (syntax-rules (rec in)
    ((_ rec x y in e ...) ((lambda ()  (define x y)  e ...)))
    ((_ rec x y    r ...) ((lambda ()  (define x y)  (let^ r ...))))
    ((_     x y in e ...) ((lambda (x) e ...)        y))
    ((_     x y    r ...) ((lambda (x) (let^ r ...)) y))))

;;; misc.
(:= √ sqrt)

(:= inc (λ (+ _ 1))) (:= ∆ inc)
(:= dec (λ (- _ 1))) (:= ∇ dec)

;; nb. nabla really isnt decr operator but
;; lets just roll with it.


;;;; *** clojure threading macros ***
(define-syntax ~>
  (syntax-rules ()
    ((_ x)               x)
    ((_ x ... (y z ...)) (y (~> x ...) z ...))
    ((_ x ... y)         (y (~> x ...)))))

(define-syntax ~>>
  (syntax-rules ()
    ((_ x)               x)
    ((_ x ... (y z ...)) (y z ... (~>> x ...)))
    ((_ x ... y)         (y (~>> x ...)))))


;;;; *** bool ***
;; Ocaml/Haskell/Coq/APL etc. mathematical notation
;; inspired in general. Used to keep question marks
;; on every predicate-function (boolean returning)
;; like in J. Shutt's Kernel, but instead opted
;; for terser code and left the ?'s to special
;; cases.

(define true  #t)
(define ⊤     #t) ; verum (down tack) (U22A4)
(define false #f)
(define ⊥     #f) ; falsum (up tack) (U22A5)

(define bool? boolean?)
(define proc? procedure?)
(define str?  string?)
(define ::?   pair?)
;; bit of cheating, these actually correspond to SV-keyboards slashed O
(define Ø?    null?)
;; cant really use curly brackets so unslashed capital o for list for now
(define O?    list?)
(define V?    vector?)

(define N? number?)
(define ℕ? number?)
(define Z? integer?)
(define ℤ? integer?)
(define R? real?)
(define ℝ? real?)
(define Q? rational?)
(define ℚ? rational?)
(define C? complex?)
(define ℂ? complex?)

(define-syntax-rule (/\ . x) (and . x))
(define-syntax-rule (∧  . x) (and . x)) ; logical'and (U2227)
(define-syntax-rule (⋀  . x) (and . x)) ; n-ary logical 'and' (U22C0)
(define-syntax-rule (\/ . x) (or  . x))
(define-syntax-rule (∨  . x) (or  . x)) ; logical 'or' (U2228)
(define-syntax-rule (⋁  . x) (or  . x)) ; n-ary logical 'or' (U22C1)
(define-syntax-rule (-. . x) (not . x))
(define-syntax-rule (¬  . x) (not . x)) ; unicode logical symbol 'not' (U00AC)
(define-syntax-rule (~  . x) (not . x))

(define-syntax-rule (/= . x) (not (= . x)))
(define-syntax-rule (/> . x) (not (> . x)))
(define-syntax-rule (/< . x) (not (< . x)))
;; negation acts as an implicit question mark
;; in these cases.
(define-syntax-rule (¬O . x) (not (list?   . x)))
(define-syntax-rule (¬Ø . x) (not (null?   . x)))
(define-syntax-rule (¬V . x) (not (vector? . x)))

;; church style succ of zero
(define =0? (λ (if (= _    0)                      ⊤ ⊥)))
(define =1? (λ (if (= _ (∆ 0))                     ⊤ ⊥)))
(define =2? (λ (if (= _ (∆ (∆ 0)))                 ⊤ ⊥)))
(define =3? (λ (if (= _ (∆ (∆ (∆ 0))))             ⊤ ⊥)))
(define =4? (λ (if (= _ (∆ (∆ (∆ (∆ 0)))))         ⊤ ⊥)))
(define =5? (λ (if (= _ (∆ (∆ (∆ (∆ (∆ 0))))))     ⊤ ⊥)))
(define =6? (λ (if (= _ (∆ (∆ (∆ (∆ (∆ (∆ 0))))))) ⊤ ⊥)))

(define (every? p xs)
  (let^ rec α (cond ((Ø? xs)     ⊤)
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

;;; if sugared / when / unless
;; denoted simply by prime to take advantage
;; of emacs font-lock / highlighting
(define-syntax if'
  (syntax-rules (then else)
    ((_ p then e else e*) (if p e e*))
    ((_ p e e*)           (if p e e*))))

(define-syntax if-not
  (syntax-rules ()
    ((_ p e e*) (if (¬ p) e e*))))

(define-syntax if-let
  (syntax-rules ()
    ((_ (x y) e e*) (let ((x y)) (if x e e*)))))

(define-syntax when
  (syntax-rules ()
    ((_ p e . r) (if p (begin e . r) ⊥))))

(define-syntax when-let
  (syntax-rules ()
    ((_ (x y) e ...) (let ((x y)) (when x e ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ p e . r) (if (¬ p) (begin e . r) ⊥))))


;;;; *** list functions ***
;; little schemer fame
(define rember
  (λ x ys =>
     (cond ((Ø? ys)       Ø)
           ((= x (hd ys)) (tl ys))
           (else          (:: (hd ys) (rember x (tl ys)))))))

;; indices should start at 1 goddamnit
(:= ι (λ (map (λ (+ _ 1)) (iota _))))

;; list-ref with idx starting at 1
(define (O-ref xs n)
  (cond ((Ø? xs) ⊥)
        ((=1? n) (hd xs))
        (else    (O-ref (tl xs) (∇ n)))))

(:= O@ O-ref)

(define (foldl f n xs)
  (if (Ø? xs) n
      (foldl f (f n (hd xs)) (tl xs))))

(:= /^>  foldl)

(define (foldr f n xs)
  (if (Ø? xs) n
      (f (hd xs) (foldr f n (tl xs)))))

(:= \^>  foldr)

(define (map f xs)
  (if (Ø? xs) Ø
      (:: (f (hd xs)) (map f (tl xs)))))

(:= $/>  map)

(define (last xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (/^> (λ x y => y) Ø xs))))

(define (rev xs)
  (let^ rec α (λ x y =>
                 (cond ((Ø? x) y)
                       ((¬O x) ⊥)
                       (else   (α (tl x) (:: (hd x) y)))))
        in (α xs Ø)))

;; less than ideal probably but just testing around
(define (zip-with f xs ys)
  (let^ rec α (λ f x y acc =>
                 (cond ((⋁ (Ø? x) (Ø? y)) (rev acc))
                       ((⋁ (¬O x) (¬O y)) ⊥)
                       (else              (α f (tl x) (tl y)
                                             (:: (f (hd x) (hd y)) acc)))))
        in (α f xs ys Ø)))

(define (zip-with3 f xs ys zs)
  (let^ rec α (λ f x y z acc =>
                 (cond ((⋁ (Ø? x) (Ø? y) (Ø? z)) (rev acc))
                       ((⋁ (¬O x) (¬O y) (¬O z)) ⊥)
                       (else
                        (α f (tl x) (tl y) (tl z)
                           (:: (f (hd x) (hd y) (hd z)) acc)))))
        in (α f xs ys zs Ø)))

(define (sum xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (/^> + 0 xs))))

(:= Σ sum)

(define (product xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (/^> * (∆ 0) xs))))

(:= ∏ product)

(define (factorial n)
  (let^ rec α (λ n acc =>
                 (if (=0? n) acc
                     (α (∇ n) (* n acc))))
        in (α n (∆ 0))))

(:= fact factorial)
(:= !    factorial)

;; Y-combinator
(define Y
  (λ f => ((λ (_ _)) (λ (f (λ a .. => ($> (_ _) a)))))))

;;;; *** vectors ***
;; vector-ref with idx starting at 1
;; TODO: implement this independtly of vector-ref
(define (V-ref xv n)
  (cond ((¬V xv) ⊥)
        (else    (vector-ref xv (∇ n)))))

(:= V@ V-ref)


;;;; *** immutability ***
(define-syntax-rule (set!     . x) (error "mutation disallowed"))
(define-syntax-rule (set-car! . x) (error "mutation disallowed"))
(define-syntax-rule (set-cdr! . x) (error "mutation disallowed"))


;;;; *** demo ***
;; two factorial definiton just to show off
(:= !'   (λ (/^> * 1 (ι _))))
(:= !''  (λ (∏ (ι _))))
