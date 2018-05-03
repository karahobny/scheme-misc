;; -*- geiser-scheme-implementation: guile -*-

(use-modules
 (ice-9 match)
 (srfi srfi-9 gnu)
 (srfi srfi-26)
 (oop goops))

;;;; *** lambda ***
;; either \ or λ to stand for enhanced lambda.
;; `->' to separate variables from the expression.
;; without variables and `->', use a single variable
;; accesed with an underscore, `_'.
;; lambda rest args works with two dots after arg now.
;; y-combinator shown as an example.
;; lambda with multiple lambdas as an argument yet to be
;; fixed, but with (λ . r) pattern matching you can simply
;; resort to basic lambda notation in those cases.

;; TODO: fix this, think recursively

;; Example:
;;  ((λ x y -> (+ x y)) 5 10) => 15
;;  ((λ (+ _ 10)) 5)          => 15

(define-syntax \
  (lambda (stx)
    (syntax-case stx (-> ..)
      ((\ (e ...))
       (with-syntax ((x (datum->syntax #'\ '_)))
         #'(lambda (x) (e ...))))
      ((_ v .. -> (e ...))  #'(lambda v (e ...)))       ; lambda rest arg case
      ((_ v ... -> (e ...)) #'(lambda (v ...) (e ...))) ; check for parens on the expr
      ((_ x y -> e ...)     #'(lambda (x y) e ...))     ; for cases like cheap `last'
      ((_ x -> e ...)       #'(lambda (x) e ...))       ; for identity
      ((_ . r)              #'(lambda . r)))))          ; regular lambda-form

(define-syntax λ
  (lambda (stx)
    (syntax-case stx (-> ..)
      ((λ (e ...))
       (with-syntax ((x (datum->syntax #'λ '_)))
         #'(lambda (x) (e ...))))
      ((_ v .. -> (e ...))  #'(lambda v (e ...)))
      ((_ v ... -> (e ...)) #'(lambda (v ...) (e ...)))
      ((_ x y -> e ...)     #'(lambda (x y) e ...))
      ((_ x -> e ...)       #'(lambda (x) e ...))
      ((_ . r)              #'(lambda . r)))))

(define Y
  (λ f -> ((λ (_ _)) (λ (f (λ a .. -> (apply (_ _) a)))))))

(define-syntax lambda^
  (lambda (stx)
    (syntax-case stx ()
      ((_ v e r ...)
       #'(λ v ->
            (call/cc
             (λ escape ->
                (syntax-parameterize
                    ((return
                      (syntax-rules ()
                        ((return vals (... ...))
                         (escape vals (... ...))))))
                  e r ...))))))))

(define-syntax-rule (λ^ . x) (lambda^ . x))


;;;; *** let ***

;; ml-style syntax for let*-closures
;; Example:
;;   (define (fact n)
;;     (let^ rec aux :=
;;           (λ n acc ->
;;              (if (= n 0) acc
;;                  (aux (- n 1) (* n acc))))
;;           and facts := (λ n -> (aux n 1))
;;           and rec aux2 :=
;;           (λ n acc ->
;;              (if (= n 0) acc
;;                  (aux2 (- n 1) (cons (facts n) acc))))
;;           in (aux2 n '())))
;;   (fact 5)
;;   => (1 2 6 24 120)

(define-syntax let^
  (lambda (stx)
    (syntax-case stx (rec := and in)
      ((_ rec x := y in  e ...)
       #'((lambda () (define x y) ((lambda (x) e ...) y))))
      ((_     x := y in  e ...)
       #'((lambda (x) e ...) y))
      ((_ rec x := y and e ...)
       #'((lambda () (define x y) (let^ e ...))))
      ((_     x := y and e ...)
       #'((lambda (x) (let^ e ...)) y)))))

;; more syntactic sugar
;; Example:
;;   (let^^ (x 10) & (y (+ x 19)) & (z (+ x y)) in z)
;;   => 39

(define-syntax let^^
  (lambda (stx)
    (syntax-case stx (& in)
      ((_ rec (x y) in  e ...)
       #'((lambda () (define x y) ((lambda (x) e ...) y))))
      ((_     (x y) in  e ...)
       #'((lambda (x) e ...) y))
      ((_ rec (x y) & e ...)
       #'((lambda () (define x y) (let^^ e ...))))
      ((_     (x y) & e ...)
       #'((lambda (x) (let^^ e ...)) y)))))

;; quick letrec thanks to dybvig
(define-syntax rec
  (lambda (stx)
    (syntax-case stx ()
      ((_ (x . v) e) #'(letrec ((x (lambda v . e))) e))
      ((_ x y)       #'(letrec ((x y)) x)))))


;;;; *** clojure threading macros ***
(define-syntax ~>
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)               #'x)
      ((_ x ... (y z ...)) #'(y (~> x ...) z ...))
      ((_ x ... y)         #'(y (~> x ...))))))

(define-syntax ~>>
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)               #'x)
      ((_ x ... (y z ...)) #'(y z ... (~>> x ...)))
      ((_ x ... y)         #'(y (~>> x ...))))))


;;;; *** bool ***
(define true  #t)
(define false #f)

(define bool? boolean?)
(define proc? procedure?)
(define str?  string?)
(define vec?  vector?)

(define N? number?)
(define Z? integer?)
(define R? real?)
(define Q? rational?)
(define C? complex?)

;; these are largely inspired by J. Shutt's
;; Kernel, with all predicate-procs ending in
;; a question mark (?). Haskell, Ocaml, Coq, Datalog
;; and mathematical notation in general has
;; also been a huge influence as seen especially
;; on /=?, /\?, \/?, -.?

(define-syntax-rule (>? . x)  (> . x))
(define-syntax-rule (<? . x)  (< . x))
(define-syntax-rule (>=? . x) (>= . x))
(define-syntax-rule (<=? . x) (<= . x))
(define-syntax-rule (=? . x)  (= . x))
(define-syntax-rule (/=? . x) (not (= . x)))

(define-syntax-rule (and? . x) (and . x))
(define-syntax-rule (/\?  . x) (and . x))
(define-syntax-rule (or?  . x) (or  . x))
(define-syntax-rule (\/?  . x) (or  . x))
(define-syntax-rule (not? . x) (not . x))
(define-syntax-rule (-.?  . x) (not . x))
(define-syntax-rule (~?   . x) (not . x))

(define 0? zero?)


;;;; *** primitives ***
;;; cxr
;; car
(define hd   car)
(define head car)
;; cdr
(define tl   cdr)
(define tail cdr)

;;; lists
(define-syntax-rule (::  . x) (cons  . x))
(define-syntax-rule (::* . x) (cons* . x))

(define null '())
(define @    append)
(define ^    string-append)

;;; println
(define (println . str)
  (for-each display str)
  (newline))

;;; if sugared / when / unless
(define-syntax if^
  (lambda (stx)
    (syntax-case stx (then else)
      ((_ p then e else e*) #'(if p e e*))
      ((_ p e e*)           #'(if p e e*)))))

(define-syntax when
  (lambda (stx)
    (syntax-case stx ()
      ((_ p e . r) #'(if p (begin e . r) false)))))

(define-syntax unless
  (lambda (stx)
    (syntax-case stx ()
      ((_ p e . r) #'(if (-.? p) (begin e . r) false)))))


;;;; *** list functions ***
(define (fold f n xs)
  (if (null? xs) n
      (fold f (f n (hd xs)) (tl xs))))

(define (foldr f n xs)
  (if (null? xs) n
      (f (hd xs) (foldr f n (tl xs)))))

(define (map f xs)
  (if (null? xs) null
      (:: (f (hd xs)) (map f (tl xs)))))

(define (last xs)
  (cond ((null? xs)       null)
        ((-.? (list? xs)) false)
        (else             (fold (λ x y -> y) null xs))))

(define (rev xs)
  (let^ rec aux :=
        (λ x y ->
           (if (null? x) y
               (aux (tl x) (:: (hd x) y))))
        in (aux xs null)))

(define (sum-ls xs)
  (cond ((null? xs)       null)
        ((-.? (list? xs)) false)
        (else             (fold (λ x y -> (+ x y)) 0 xs))))

(define (prod-ls xs)
  (cond ((null? xs)       null)
        ((-.? (list? xs)) false)
        (else             (fold (λ x y -> (* x y)) 1 xs))))

(define ι iota)

(define ι1
  (λ (map (λ (+ _ 1)) (ι _))))

(define (fact n)
  (let^ rec aux :=
        (λ n acc ->
           (if (= n 0) acc
               (aux (- n 1) (* n acc))))
        in (aux n 1)))

(define fldfac
  (λ (fold * 1 (ι1 _))))

(load "cxr.scm")
