;; -*- geiser-scheme-implementation: guile -*-
(add-to-load-path "/home/jamppa/src/scheme/")
(add-to-load-path "/home/jamppa/src/scheme/guile-monads")
(add-to-load-path "/home/jamppa/git/scheme-misc/lazar")
(add-to-load-path "/home/jamppa/git/scheme-misc/lazar/lazar")

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
             (lazar)
             (lazar  contracts)
             (lazar  fn-lambda)
             (lazar  case-with)
             (lazar  est)
             (lazar  contracts)
             (oop    goops))

(read-enable  'r7rs-symbols)
(print-enable 'r7rs-symbols)


;;;; *** list functions ***
;; little schemer fame
(define/c (rember ((x : α) (ys : List)) -> List)
  (case-with || (Ø? ys)       => Ø
             || (= x (hd ys)) => (tl ys)
             || else          => (:: (hd ys) (rember x (tl ys)))))

;; indices should start at 1 goddamnit
(define/c (ι (n : ℕ) -> List)
  (map (λ (+ _ 1)) (iota n)))

;; list-ref with idx starting at 1
(define/c (list-ref ((xs : List) (n : ℕ)) -> α)
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

(define/c (sum (xs : (ListOf Num)) -> Num)
  (case-with || (Ø? xs) => Ø || (¬O xs) => ⊥ || else => (foldl + 0 xs)))

(define Σ sum)

(define/c (product (xs : (ListOf Num)) -> Num)
  (case-with || (Ø? xs) => Ø || (¬O xs) => ⊥ || else => (foldl * 1 xs)))

(define ∏ product)

(define/c (list# (xs : List) -> ℕ)
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
  (λ f => ((λ (_ _)) (λ (f (λ a .. => (<$> (_ _) a)))))))


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
