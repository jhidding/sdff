; ~/~ begin <<docs/monads.md#scheme/monads/support.scm>>[init]
(library (monads support)
  (export seq-map)
  (import (rnrs (6))
          (monads syntax))

  (define (seq-map M f . args)
    (assert (not (null? args)))
    (let loop ((a args)
               (b '()))
      (if (null? (car a))
        ((monad-return M) (reverse b))
        (seq M
          (x <- (apply f (map car a)))
          (loop (map cdr a) (cons x b))))))

  (define (seq-compose M . fs)
    (let ((c2 (lambda (f g) (lambda (x) ((monad-bind M) (g x) f)))))
      (fold-right c2 values fs)))
)
; ~/~ end
