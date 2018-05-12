(library (lazar)
  (export
   ;; aux keywords λ.=> est ε case-with)
   => .. || else rec in
   ;; syntax
   λ λ.=> ε case-with Pair ListOf Vec VecOf
   Fn Bool Symbol Char Str Num Natural
   natural? Int Real Rational Complex Any
   a' α ℕ ℤ ℝ ℚ ℂ : -> define/c)
  (import (rnrs (6))
          (lazar fn-lambda)
          (lazar est)
          (lazar case-with)
          (lazar contracts)))
