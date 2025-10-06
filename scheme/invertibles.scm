; ~/~ begin <<docs/ch2-dsl.md#scheme/invertibles.scm>>[init]
(library (invertibles)
  (import (rnrs)
          (only (chezscheme)
              make-wrapper-procedure
              wrapper-procedure-procedure
              wrapper-procedure-data))

  (export (make-invertible invertible? invert))

  (define-record-type invertible-data
      (fields inverse))

  (define (make-invertible a b)
      (make-wrapper-procedure
          a 2 (make-invertible-data b)))

  (define (invertible? f)
      (and (wrapper-procedure? f)
           (invertible-data? (wrapper-procedure-data f))))

  (define (invert f)
      (let ((a (wrapper-procedure-procedure f))
            (b (invertible-data-inverse (wrapper-procedure-data f))))
          (make-invertible b a)))
)
; ~/~ end
