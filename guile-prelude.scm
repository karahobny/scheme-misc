;; -*- geiser-scheme-implementation: guile -*-

;; general abbreviations on macro patterns to match are:
;; x, y, z, a, b, c => single variables or values. a can also stand for args.
;; v ...             => multiple variables or values
;; e (...)           => expression(s)
;; (.) r            => rest of the pattern

;; asterisks are there only to denote multiple of the
;; corresponding pattern.


(use-modules (ice-9 match)
             (ice-9 format)
             (srfi  srfi-1)
             (srfi  srfi-9  gnu)
             (srfi  srfi-26)
             (srfi  srfi-41)
             (oop   goops))

(read-enable  'r7rs-symbols)
(print-enable 'r7rs-symbols)


;;;; *** lambda ***
;; either \ or λ to stand for enhanced lambda.
;; `->' to separate variables from the expression.
;; without variables and `->', uses a single variable
;; accesed with an underscore, `_'.

;; lambda rest args works with two dots after arg now.
;; y-combinator shown as an example.

;; with (λ . r) pattern matching you can simply
;; resort to basic lambda notation in those cases.

;; Example:
;;  ((λ x y -> (+ x y)) 5 10) => 15
;;  ((λ (+ _ 10)) 5)          => 15

(define-syntax \
  (lambda (stx)
    (syntax-case stx (-> ..)
      ((\ (e ...))
       (with-syntax ((x (datum->syntax #'\ '_))) #'(lambda (x) (e ...))))
      ((_ v ..  -> (e ...)) #'(lambda v (e ...)))       ; lambda rest arg case
      ((_ v ... -> (e ...)) #'(lambda (v ...) (e ...))) ; check for parens on the expr
      ((_ x y   ->  e ...)  #'(lambda (x y) e ...))     ; for cases like cheap `last'
      ((_ x     ->  e ...)  #'(lambda (x) e ...))       ; id
      ((_       ->  e ...)  #'(lambda () e ...))
      ((_ . r)              #'(lambda . r)))))          ; regular lambda-form

(define-syntax λ
  (lambda (stx)
    (syntax-case stx (-> ..)
      ((λ (e ...))
       (with-syntax ((x (datum->syntax #'λ '_))) #'(lambda (x) (e ...))))
      ((_ v ..  -> (e ...)) #'(lambda v (e ...)))
      ((_ v ... -> (e ...)) #'(lambda (v ...) (e ...)))
      ((_ x y   ->  e ...)  #'(lambda (x y) e ...))
      ((_ x     ->  e ...)  #'(lambda (x) e ...))
      ((_       ->  e ...)  #'(lambda () e ...))
      ((_ . r)              #'(lambda . r)))))

(define Y
  (λ f -> ((λ (_ _)) (λ (f (λ a .. -> (apply (_ _) a)))))))

(define-syntax lambda^
  (lambda (stx)
    (syntax-case stx ()
      ((_ v e e* ...)
       #'(λ v ->
            (call/cc
             (λ escape ->
                (syntax-parameterize
                    ((return
                      (syntax-rules ()
                        ((return vals (... ...))
                         (escape vals (... ...))))))
                  e e* ...))))))))

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
    (syntax-case stx (rec :=)
      ((_ rec x := y c e ...)
       (cond
        ((and (identifier? #'x) (free-identifier=? #'c #'in))
         #'((lambda () (define x y) ((lambda (x) e ...) y))))
        ((and (identifier? #'x) (free-identifier=? #'c #'and))
         #'((lambda () (define x y) (let^ e ...))))))
      ((_     x := y c e ...)
       (cond
        ((and (identifier? #'x) (free-identifier=? #'c #'in))
         #'((lambda (x)       e ...)  y))
        ((and (identifier? #'x) (free-identifier=? #'c #'and))
         #'((lambda (x) (let^ e ...)) y)))))))

;; more syntactic sugar
;; Example:
;;   (let^^ (x 10) & (y (+ x 19)) & (z (+ x y)) in z)
;;   => 39

(define-syntax let^^
  (lambda (stx)
    (syntax-case stx (^ in)
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
(define ⊤     #t) ; verum (down tack) (U22A4)
(define false #f)
(define ⊥     #f) ; falsum (up tack) (U22A5)

(define bool? boolean?)
(define proc? procedure?)
(define str?  string?)
(define vec?  vector?)
(define ::?   pair?)
;; bit of cheating, these actually correspond to SV-keyboards slashed O
(define Ø?    null?)
;; cant really use curly brackets so unslashed capital o for list for now
(define O?    list?)

(define N? number?)
(define Z? integer?)
(define R? real?)
(define Q? rational?)
(define C? complex?)

(define-syntax-rule (>  . x) (>  . x))
(define-syntax-rule (>= . x) (>= . x))
(define-syntax-rule (<  . x) (<  . x))
(define-syntax-rule (<= . x) (<= . x))
(define-syntax-rule (=  . x) (=  . x))


(define-syntax-rule (/\ . x) (and . x))
(define-syntax-rule (∧  . x) (and . x)) ; unicode logical symbol 'and' (U2227)
(define-syntax-rule (\/ . x) (or  . x))
(define-syntax-rule (∨  . x) (or  . x)) ; unicode logical symbol 'or' (U2228)
(define-syntax-rule (-. . x) (not . x))
(define-syntax-rule (¬  . x) (not . x)) ; unicode logical symbol 'not' (U00AC)
(define-syntax-rule (~  . x) (not . x))

(define-syntax-rule (/= . x) (not (= . x)))
(define-syntax-rule (/> . x) (not (> . x)))
(define-syntax-rule (/< . x) (not (< . x)))
(define-syntax-rule (¬O . x) (not (list? . x)))
(define-syntax-rule (¬Ø . x) (not (null? . x)))

(define (every? p xs)
  (let^ rec aux :=
        (cond ((Ø? xs)     ⊤)
              ((p (hd xs)) (aux (tl xs)))
              (else        ⊥))
        in (aux p xs)))


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
(define Ø    '())
(define O    list)

;;; println
(define \n newline)
(define (println . str)
  (for-each display str)
  (\n))

;;; if sugared / when / unless
;; denoted simply by prime to take advantage
;; of emacs font-lock / highlighting
(define-syntax if'
  (lambda (stx)
    (syntax-case stx (then else)
      ((_ p then e else e*) #'(if p e e*))
      ((_ p e e*)           #'(if p e e*)))))

(define-syntax if-not
  (lambda (stx)
    (syntax-case stx ()
      ((_ p e e*) #'(if (¬ p) e e*)))))

(define-syntax if-let
  (lambda (stx)
    (syntax-case stx (:= in)
      ((_ (x y) e e*)     #'(let ((x y)) (if x e e*)))
      ((_ x := y in e e*) #'(let^ x := y in (if x e e*))))))

(define-syntax when
  (lambda (stx)
    (syntax-case stx ()
      ((_ p e . r) #'(if p (begin e . r) ⊥)))))

(define-syntax when-let
  (lambda (stx)
    (syntax-case stx (:= in)
      ((_ (x y) e ...)     #'(let ((x y)) (when x e ...)))
      ((_ x := y in e ...) #'(let^ x := y in (when x e ...))))))

(define-syntax unless
  (lambda (stx)
    (syntax-case stx ()
      ((_ p e . r) #'(if (¬ p) (begin e . r) ⊥)))))


;;;; *** list functions ***
(define (foldl f n xs)
  (if (Ø? xs) n
      (foldl f (f n (hd xs)) (tl xs))))

(define (foldr f n xs)
  (if (Ø? xs) n
      (f (hd xs) (foldr f n (tl xs)))))

(define (map f xs)
  (if (Ø? xs) Ø
      (:: (f (hd xs)) (map f (tl xs)))))

(define (last xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (foldl (λ x y -> y) Ø xs))))

(define (rev xs)
  (let^ rec aux :=
        (λ x y ->
           (cond ((Ø? x) y)
                 ((¬O x) ⊥)
                 (else   (aux (tl x) (:: (hd x) y)))))
        in (aux xs Ø)))

;; less than ideal probably but just testing around
(define (zip-with f xs ys)
  (let^ rec aux :=
        (λ f x y acc ->
           (cond ((∨ (Ø? x) (Ø? y)) (rev acc))
                 ((∨ (¬O x) (¬O y)) ⊥)
                 (else (aux f (tl x) (tl y) (:: (f (hd x) (hd y)) acc)))))
        in (aux f xs ys Ø)))

(define (zip-with3 f xs ys zs)
  (let^ rec aux :=
        (λ f x y z acc ->
           (cond
            ((∨ (Ø? x) (Ø? y) (Ø? z)) (rev acc))
            ((∨ (¬O x) (¬O y) (¬O z)) ⊥)
            (else (aux f (tl x) (tl y) (tl z) (:: (f (hd x) (hd y) (hd z)) acc)))))
        in (aux f xs ys zs Ø)))

(define (sum xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (foldl + 0 xs))))

(define Σ sum)

(define (product xs)
  (cond ((Ø? xs) Ø)
        ((¬O xs) ⊥)
        (else    (foldl * 1 xs))))

(define ∏ product)

(define √ sqrt)

;; indices should start at 1 goddamnit
(define ι
  (λ (map (λ (+ _ 1)) (iota _))))

(define (list-ref xs n)
  (cond ((Ø? xs)  ⊥)
        ((= n 1)  (hd xs))
        (else     (list-ref (tl xs) (- n 1)))))

(define O-ref list-ref)

(define (fact n)
  (let^ rec aux :=
        (λ n acc ->
           (if (= n 0) acc
               (aux (- n 1) (* n acc))))
        in (aux n 1)))

(define foldfact (λ (foldl * 1 (ι _))))

;; apl-like demonstration of terse factorial definition
(define ! (λ (∏ (ι _))))

(load "cxr.scm")
