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
    (syntax-case stx( => ..)
      ((λ (e ...))
       (with-syntax ((x (datum->syntax #'λ '_))) #'(lambda (x) (e ...))))
      ((_ v ..  => (e ...)) #'(lambda v (e ...)))
      ((_ v ... => (e ...)) #'(lambda (v ...) (e ...)))
      ((_ x y   =>  e ...)  #'(lambda (x y) e ...))
      ((_ x     =>  e ...)  #'(lambda (x) e ...))
      ((_ . r)              #'(lambda . r)))))

;; temporary solution for thunk
(define-syntax λ.=>
  (syntax-rules ()
    ((_ . x) (lambda () . x))))

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


;;;; *** exceptions ***
(define-syntax-rule (raise-stx . x) (syntax-error . x))


;;;; *** primitives ***
;;; cons
(define-syntax-rule (::  . x) (cons  . x))
(define-syntax-rule (::* . x) (cons* . x))

;;; apply / compose
(define <$> apply)

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

(define O->V list->vector)
(define V->O vector->list)

;;; misc.
(define inc (λ (+ _ 1)))
(define dec (λ (- _ 1)))

(define ∆ inc)
(define ∇ dec)
;; nb. nabla really isnt decr operator but
;; lets just roll with it.
(define √ sqrt)


;;;; *** let ***

;; Example:
;;         (est α 10 β 20 γ (+ α β) in γ)
;;    or:  (ε α (λ x => (+ x 10)) β 20 in (α β))
;; Use linebreaks atm to differentiate between the vars
;; and their binds from each other if you wish.

;; TODO: named-let

(define-syntax rec
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'rec)))

(define-syntax in
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'in)))

(define-syntax est
  (syntax-rules (rec in)
    ;; letrec*
    ((_ rec x y in e ...) ((lambda ()  (define x y)  e ...)))
    ((_ rec x y    r ...) ((lambda ()  (define x y)  (est r ...))))
    ;; let*
    ((_     x y in e ...) ((lambda (x) e ...)        y))
    ((_     x y    r ...) ((lambda (x) (est r ...))  y))))

;; like peano used to indicate something as an element
;; of a set with an ε (greek letter) as in `est` (is),
;; might as well use this too to indicate let closures
(define-syntax-rule (ε . x) (est . x))

(define-syntax let
  (syntax-rules ()
    ((_ ((x y) ...) r ...)
     ((lambda (x ...) r ...) y ...))
    ((_ x ((y z) ...) r ...)
     ((lambda () (define x (lambda (y ...) r ...)) (x z ...))))))

(define-syntax let*
  (syntax-rules ()
    ((_ () e ...)
     ((lambda () e ...)))
    ((let* ((x y) r ...) e ...)
     ((lambda (x) (let* (r ...) e ...)) y))))


;;;; *** sugared cond ***
(define-syntax ||
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'||)))

(define-syntax =>
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'=>)))

(define-syntax else
  (identifier-syntax
   (syntax-violation #f "misplaced aux keyword" #'else)))

;; very SML-like cond/case
;; the => aux keyword works purely as a syntactic sugar and it'll work
;; fine without it.

;; Example:
;; (define/c (product (xs : (ListOf ℕ)) -> ℕ))
;;   (case-with || (Ø? xs) => Ø
;;              || (¬O xs) => ⊥
;;              || else    => (foldl * 1 xs)

(define-syntax case-with
  (syntax-rules (|| => else)
    ((_ || x => y || e ...) (if x y (case-with e ...)))
    ((_    x => y || e ...) (if x y (case-with e ...)))
    ((_ || x      || e ...) (syntax-error "Clause has no expression to test for" x))
    ((_    x      || e ...) (syntax-error "Clause has no expression to test for" x))
    ((_ || x    y || e ...) (if x y (case-with e ...)))
    ((_    x    y || e ...) (if x y (case-with e ...)))
    ((_    x      || e ...) (syntax-error "Clause has no expression to test for" x))
    ((_ else => x)          (if #t x #f))
    ((_ else    x)          (if #t x #f))
    ((_ || x => y)          (if x y #f))
    ((_    x => y)          (if x y #f))
    ((_ || x    y)          (if x y #f))
    ((_ || x)               (syntax-error "Clause has no expression to test for" x))
    ((_    x    y)          (if x y #f))
    ((_    x)               (syntax-error "Clause has no expression to test for" x))))

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
(define fn?   procedure?)
(define str?  string?)
(define ::?   pair?)
;; bit of cheating, these actually correspond to SV-keyboards slashed O
(define Ø?    null?)
;; cant really use curly brackets so unslashed capital o for list for now
(define O?  list?)
(define V?  vector?)
(define *V? bitvector?)

(load "logic.scm")

(define (seq? xx)
  (⋁ (O? xx) (V? xx) (string? xx) (hash-table? xx) (*V? xx) (stream? xx)))

;; negation of numerical tower
(define-syntax-rule (¬N . x) (not (number?   . x)))
(define-syntax-rule (¬ℕ . x) (not (number?   . x)))
(define-syntax-rule (¬Z . x) (not (integer?  . x)))
(define-syntax-rule (¬ℤ . x) (not (integer?  . x)))
(define-syntax-rule (¬ℝ . x) (not (real?     . x)))
(define-syntax-rule (¬R . x) (not (real?     . x)))
(define-syntax-rule (¬ℚ . x) (not (rational? . x)))
(define-syntax-rule (¬Q . x) (not (rational? . x)))
(define-syntax-rule (¬ℂ . x) (not (complex?  . x)))
(define-syntax-rule (¬C . x) (not (complex?  . x)))

;; negation of lists, vectors etc.
(define-syntax-rule (¬O . x) (not (list?   . x)))
(define-syntax-rule (¬Ø . x) (not (null?   . x)))
(define-syntax-rule (¬V . x) (not (vector? . x)))

(define 0? (λ (if (= _    0)  ⊤ ⊥)))
(define 1? (λ (if (= _ (∆ 0)) ⊤ ⊥)))

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


;;;; *** contracts ***
;; Copyright 2017- Linus Björstam
;; with little modifications by me

;; Example:
;; (define/c (sum (xs : (ListOf ℕ)) -> ℕ)
;;  (case-with || (Ø? xs) => Ø || (¬O xs) => ⊥ || else => (foldl + 0 xs)))

;; Currently supports types:
;; Fn (procedure?), List (list?), Num (number?)
;; Int (integer?), Str (string?), Any/α/a' (anything)
;; ListOf x (checks predicate for every member of list)
;; VectorOf x (checks predicate for every member of list)

(load "contracts.scm")


;;;; *** list functions ***
;; little schemer fame
(define/c (rember ((x : α) (ys : List)) -> List)
  (case-with || (Ø? ys)       => Ø
             || (= x (hd ys)) => (tl ys)
             || else          => (:: (hd ys) (rember x (tl ys)))))

;; indices should start at 1 goddamnit
(define/c (ι (n : ℤ) -> List)
  (map (λ (+ _ 1)) (iota n)))

;; list-ref with idx starting at 1
(define/c (O-ref ((xs : List) (n : ℤ)) -> α)
  (case-with || (Ø? xs) => ⊥
             || (1? n)  => (hd xs)
             || else    => (O-ref (tl xs) (∇ n))))

(define/c (foldl ((f : Fn) (n : α) (xs : List)) -> α)
  (case-with || (Ø? xs) => n
             || else    => (foldl f (f n (hd xs)) (tl xs))))

(define/c (foldr ((f : Fn) (n : α) (xs : List)) -> α)
  (case-with || (Ø? xs) => n
             || else    => (f (hd xs) (foldr f n (tl xs)))))

(define/c (map ((f : Fn) (xs : List)) -> List)
  (case-with || (Ø? xs) => Ø
             || else    => (:: (f (hd xs)) (map f (tl xs)))))

(define/c (last (xs : List) -> α)
  (case-with || (Ø? xs) => Ø
             || else    => (foldl (λ x y => y) Ø xs)))

(define/c (rev (xs : List) -> List)
  (ε rec α
     (λ x y =>
        (case-with || (Ø? x) => y
                   || else   => (α (tl x) (:: (hd x) y))))
     in (α xs Ø)))

;; less than ideal probably but just testing around
(define/c (zip-with ((f : Fn) (xs : List) (ys : List)) -> List)
  (ε rec α
     (λ f x y acc =>
        (case-with || (⋁ (Ø? x) (Ø? y)) => (rev acc)
                   || else              => (α f (tl x) (tl y)
                                            (:: (f (hd x) (hd y)) acc))))
     in (α f xs ys Ø)))

(define/c (zip-with3 ((f : Fn) (xs : List) (ys : List) (zs : List)) -> List)
  (ε rec α
     (λ f x y z acc =>
        (case-with || (⋁ (Ø? x) (Ø? y) (Ø? z)) => (rev acc)
                   || else => (α f (tl x) (tl y) (tl z)
                               (:: (f (hd x) (hd y) (hd z)) acc))))
     in (α f xs ys zs Ø)))

(define/c (sum (xs : (ListOf ℕ)) -> ℕ)
  (case-with || (Ø? xs) => Ø || (¬O xs) => ⊥ || else => (foldl + 0 xs)))

(define Σ sum)

(define/c (product (xs : (ListOf ℕ)) -> ℕ)
  (case-with || (Ø? xs) => Ø || (¬O xs) => ⊥ || else => (foldl * 1 xs)))

(define ∏ product)

(define/c (O# (xs : List) -> ℤ)
  (ε rec α (λ xs acc =>
              (if (Ø? xs) acc
                  (α (tl xs) (∆ acc))))
     in (α xs 0)))


(define/c (factorial (n : ℕ) -> ℕ)
  (ε rec α (λ n acc =>
              (if (0? n) acc
                  (α (∇ n) (* n acc))))
     in (α n 1)))

(define fact factorial)

;; Y-combinator
(define Y
  (λ f => ((λ (_ _)) (λ (f (λ a .. => (apply (_ _) a)))))))

;;;; *** vectors ***

;; REFACTOR: probably better way to do this than to
;;           translate vector to list and using O-ref.

;; vector-ref with idx starting at 1
;; xv => vector, n => integer, return-val => any
(define/c (V-ref ((xv : Vec) (n : ℤ)) -> α)
  (O-ref (V->O xv) n))

(define V@ V-ref)

;; vector-length
(define/c (V# (xv : Vec) -> ℤ)
  (O# (V->O xv)))


;;;; *** immutability ***
(define-syntax-rule (set!     . x) (error "side effects / mutation not allowed!"))
(define-syntax-rule (set-car! . x) (error "side effects / mutation not allowed!"))
(define-syntax-rule (set-cdr! . x) (error "side effects / mutation not allowed!"))

;;; monadic println
(define (println str)
  (run-io (iodisplay str) (ionewline)))

;;;; *** demo ***
;; two factorial definiton just to show off
(define !'  (λ (foldl * 1 (ι _))))
(define !'' (λ (∏ (ι _))))
