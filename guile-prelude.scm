;; -*- geiser-scheme-implementation: guile -*-

;;;; *** lambda ***
;; either \ or λ to stand for enhanced lambda.
;; `->' to separate variables from the expression.
;; without variables and `->', use a single variable
;; accesed with an underscore, `_'.
;; currently doesnt work with paren-less lambda argument.
;; (y-combinator as an example of this.)

;; Example:
;;  ((λ x y -> (+ x y)) 5 10) => 15
;;  ((λ (+ _ 10)) 5)          => 15

;; Y-combinator:
;; (define Y
;;   (λ h ->
;;      ((λ (_ _))
;;       (λ (h (lambda args (apply (_ _) args)))))))

(define-syntax \
  (lambda (stx)
    (syntax-case stx (->)
      [(\ (e ...))
       (with-syntax ([x (datum->syntax #'\ '_)])
         #'(lambda (x) (e ...)))]
      [(_ v ... -> e)
       #'(lambda (v ...) e)]
      [(_ (v ...) (e ...))
       #'(lambda (v ...) (e ...))])))

(define-syntax λ
  (lambda (stx)
    (syntax-case stx (->)
      [(λ (e ...))
       (with-syntax ([x (datum->syntax #'λ '_)])
         #'(lambda (x) (e ...)))]
      [(_ v ... -> e)
       #'(lambda (v ...) e)]
      [(_ (v ...) (e ...))
       #'(lambda (v ...) (e ...))])))

(define-syntax lambda^
  (lambda (stx)
    (syntax-case stx ()
      [(_ v e r ...)
       #'(lambda v
           (call/cc
             (lambda (escape)
               (syntax-parameterize
                   ([return
                     (syntax-rules ()
                       [(return vals (... ...))
                        (escape vals (... ...))])])
                 e r ...))))])))

(define-syntax-rule (λ^ . x) (lambda^ . x))

(define curry (λ x y -> (f (x y))))


((curry map 1+ '(1 5 2 3 4))

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
      [(_ rec x := y in  e ...)
       #'((lambda () (define x y) ((lambda (x) e ...) y)))]
      [(_     x := y in  e ...)
       #'((lambda (x) e ...) y)]
      [(_ rec x := y and e ...)
       #'((lambda () (define x y) (let^ e ...)))]
      [(_     x := y and e ...)
       #'((lambda (x) (let^ e ...)) y)])))

;; more syntactic sugar
;; Example:
;;   (let^^ (x 10) & (y (+ x 19)) & (z (+ x y)) in z)
;;   => 39

(define-syntax let^^
  (lambda (stx)
    (syntax-case stx (& in)
      [(_ rec (x y) in  e ...)
       #'((lambda () (define x y) ((lambda (x) e ...) y)))]
      [(_     (x y) in  e ...)
       #'((lambda (x) e ...) y)]
      [(_ rec (x y) &   e ...)
       #'((lambda () (define x y) (let^^ e ...)))]
      [(_     (x y) &   e ...)
       #'((lambda (x) (let^^ e ...)) y)])))



;; quick letrec thanks to dybvig
(define-syntax rec
  (lambda (stx)
    (syntax-case stx ()
      [(_ (x . v) e)
       #'(letrec ([x (lambda v . e)]) e)]
      [(_ x y)
       #'(letrec ([x y]) x)])))


;;;; *** clojure threading macros ***
(define-syntax ~>
  (lambda (stx)
    (syntax-case stx ()
      [(_ x)
       #'x]
      [(_ x ... (y z ...))
       #'(y (~> x ...) z ...)]
      [(_ x ... y)
       #'(y (~> x ...))])))

(define-syntax ~>>
  (lambda (stx)
    (syntax-case stx ()
      [(_ x)
       #'x]
      [(_ x ... (y z ...))
       #'(y z ... (~>> x ...))]
      [(_ x ... y)
       #'(y (~>> x ...))])))


;;;; *** bool ***
(define bool? boolean?)
(define proc? procedure?)
(define str? string?)
(define vec? vector?)

(define N? number?)
(define Z? integer?)
(define R? real?)
(define Q? rational?)
(define C? complex?)

(define-syntax-rule (>? . x) (> . x))
(define-syntax-rule (<? . x) (< . x))
(define-syntax-rule (>=? . x) (>= . x))
(define-syntax-rule (<=? . x) (<= . x))
(define-syntax-rule (=? . x) (= . x))
(define-syntax-rule (/=? . x) (not (= . x)))

(define-syntax-rule (and? . x) (and . x))
(define-syntax-rule (or? . x) (or . x))
(define-syntax-rule (&&? . x) (and . x))
(define-syntax-rule (||? . x) (or . x))
(define-syntax-rule (not? . x) (not . x))
(define-syntax-rule (-.? . x) (not . x))
(define-syntax-rule (~.? . x) (not . x))
;; cons
(define-syntax-rule (:: . x) (cons . x))
;; empty list
(define null '())

;;;; *** cxr ***
;; car
(define hd   car)
(define head car)
;; cdr
(define tl   cdr)
(define tail cdr)


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
  (cond
   [(null? xs)       null]
   [(-.? (list? xs)) #f]
   [else             (fold (λ x y -> y) null xs)]))

(define (rev xs)
  (let^ rec aux :=
        (λ x y ->
           (if (null? x) y
               (aux (tl x) (:: (hd x) y))))
        in (aux xs null)))

(define (sum-ls xs)
  (cond
   [(null? xs)       null]
   [(-.? (list? xs)) #f]
   [else             (fold (λ x y -> (+ x y)) 0 xs)]))

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


;;;; *** cxxr ***
;; caar
(define (h.h x)
  (hd (hd x)))
(define (h.hd x)
  (hd (hd x)))
;; cadr
(define (h.t x)
  (hd (tl x)))
(define (h.tl x)
  (hd (tl x)))
;; cdar
(define (t.h x)
  (tl (hd x)))
(define (t.hd x)
  (tl (hd x)))
;; cddr
(define (t.t x)
  (tl (tl x)))
(define (t.tl x)
  (tl (tl x)))

;;;; *** cxxxr ***
;; caaar
(define (h.h.h x)
  (hd (hd (hd x))))
(define (h.h.hd x)
  (hd (hd (hd x))))
;; caadr
(define (h.h.t x)
  (hd (hd (tl x))))
(define (h.h.tl x)
  (hd (hd (tl x))))
;; cadar
(define (h.t.h x)
  (hd (tl (hd x))))
(define (h.t.hd x)
  (hd (tl (hd x))))
;; cdaar
(define (t.h.h x)
  (tl (hd (hd x))))
(define (t.h.hd x)
  (tl (hd (hd x))))
;; cddar
(define (t.t.h x)
  (tl (tl (hd x))))
(define (t.t.hd x)
  (tl (tl (hd x))))
;; caddr
(define (h.t.t x)
  (hd (tl (tl x))))
(define (h.t.tl x)
  (hd (tl (tl x))))
;; cdddr
(define (t.t.t x)
  (tl (tl (tl x))))
(define (t.t.tl x)
  (tl (tl (tl x))))

;;;; *** cxxxxr ***
;; caaaar
(define (h.h.h.h x)
  (hd (hd (hd (hd x)))))
(define (h.h.h.hd x)
  (hd (hd (hd (hd x)))))
;; caaadr
(define (h.h.h.t x)
  (hd (hd (hd (tl x)))))
(define (h.h.h.tl x)
  (hd (hd (hd (tl x)))))
;; caaddr
(define (h.h.t.t x)
  (hd (hd (tl (tl x)))))
(define (h.h.t.tl x)
  (hd (hd (tl (tl x)))))
;; cadddr
(define (h.t.t.t x)
  (hd (tl (tl (tl x)))))
(define (h.t.t.tl x)
  (hd (tl (tl (tl x)))))
;; cddddr
(define (t.t.t.t x)
  (tl (tl (tl (tl x)))))
(define (t.t.t.tl x)
  (tl (tl (tl (tl x)))))
;; cdaaar
(define (t.h.h.h x)
  (tl (hd (hd (hd x)))))
(define (t.h.h.hd x)
  (tl (hd (hd (hd x)))))
;; cddaar
(define (t.t.h.h x)
  (tl (tl (hd (hd x)))))
(define (t.t.h.hd x)
  (tl (tl (hd (hd x)))))
;; cdddar
(define (t.t.t.h x)
  (tl (tl (tl (hd x)))))
(define (t.t.t.hd x)
  (tl (tl (tl (hd x)))))
;; cdadar
(define (t.h.t.h x)
  (hd (tl (hd (tl x)))))
(define (t.h.t.hd x)
  (hd (tl (hd (tl x)))))
;; cadadr
(define (h.t.h.t x)
  (hd (tl (hd (tl x)))))
(define (h.t.h.tl x)
  (hd (tl (hd (tl x)))))
;; cadaar
(define (h.t.h.h x)
  (hd (tl (hd (hd x)))))
(define (h.t.h.hd x)
  (hd (tl (hd (hd x)))))
;; caadar
(define (h.t.h.t x)
  (hd (hd (tl (hd x)))))
(define (h.t.h.tl x)
  (hd (hd (tl (hd x)))))
;; cdaadr
(define (t.h.h.t x)
  (tl (hd (hd (tl x)))))
(define (t.h.h.tl x)
  (tl (hd (hd (tl x)))))
;; caddar
(define (h.t.t.h x)
  (hd (tl (tl (hd x)))))
(define (h.t.t.hd x)
  (hd (tl (tl (hd x)))))
