; ~/~ begin <<docs/ch2-4-checkers.md#scheme/utility.scm>>[init]
(library (utility)
  (export range unit-range cartesian-product)
  (import (rnrs) (combinators))

  ; ~/~ begin <<docs/ch2-4-checkers.md#scheme-utility>>[init]
  (define (range a b step)
     (do ((x a (+ x step))
          (r '() (cons x r)))
         ((>= x b) (reverse r))))

  (define (unit-range a b) (range a b 1))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-4-checkers.md#scheme-utility>>[1]
  (define cartesian-product-2 (curry (f as bs)
    (apply append (map (lambda (a) (map (lambda (b) (f a b)) bs)) as))))

  (define (cartesian-product . args)
    (fold-right (cartesian-product-2 cons) '(()) args))
  ; ~/~ end
)
; ~/~ end
