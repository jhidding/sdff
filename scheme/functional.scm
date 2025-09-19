; ~/~ begin <<docs/ch2-dsl.md#scheme/functional.scm>>[init]
;| file: scheme/functional.scm
(define-library (functional)
  (export compose identity iterate parallel-combine)
  (import (rnrs)
          (only (chezscheme) procedure-arity-mask make-wrapper-procedure))

  ; ~/~ begin <<docs/ch2-dsl.md#functional>>[init]
  ;| id: functional
  (define identity values)
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#functional>>[1]
  ;| id: functional
  (define (compose . fs)
    (fold-right compose-2 identity fs))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#functional>>[2]
  ;| id: functional
  (define (iterate n)
    (lambda (f)
      (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f)))))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#functional>>[3]
  ;| id: functional
  (define (compose-2 f g)
    (let ((arity-mask (procedure-arity-mask g)))
      (make-wrapper-procedure
        (lambda args
          (call-with-values
            (lambda () (apply g args))
            f))
        arity-mask
        #f)))

  (define (thunk? proc) (not (zero? (bitwise-and 1 (arity-mask (procedure-arity-mask proc))))))
  (define (all pred xs) (not (find (compose not pred) xs)))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#functional>>[4]
  ;| id: functional
  (define (xor a b) (if a (not b) b))

  (define (parallel-combine h . fs)
    (if (thunk? h)
      (begin (assert (null? fs)) h)
      (begin
        (assert (and
            (not (null? fs))
            (not (zero? (bitwise-and (procedure-arity-mask h)
                                     (bitwise-arithmetic-shift-left 1 (length fs)))))))
        (let ((arity-mask (fold-left bitwise-and -1 (map procedure-arity-mask fs))))
          (assert (not (zero? arity-mask)))
          (make-wrapper-procedure
            (lambda args
              (apply h (map (lambda (f) (apply f args)) fs)))
              arity-mask
              #f)))))
  ; ~/~ end
)
; ~/~ end
