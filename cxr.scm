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
