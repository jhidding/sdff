; ~/~ begin <<docs/ch2-dsl.md#scheme/combinators.scm>>[init]
(library (combinators)
  (export
      ; ~/~ begin <<docs/ch2-dsl.md#combinators-export>>[init]
      compose identity iterate parallel parallel-combine vmap const melt curry curry-helper
      ; ~/~ end
  )
  (import (rnrs)
      ; ~/~ begin <<docs/ch2-dsl.md#combinators-import>>[init]
      (only (chezscheme) procedure-arity-mask make-wrapper-procedure)
      ; ~/~ end
  )
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[init]
  (define identity values)
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[1]
  (define (compose . fs)
    (fold-right compose-2 identity fs))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[2]
  (define (vmap f)
      (lambda args
          (apply values (map f args))))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[3]
  (define (iterate n)
    (lambda (f)
      (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f)))))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[4]
  (define (compose-2 f g)
    (let ((arity-mask (procedure-arity-mask g)))
      (make-wrapper-procedure
        (lambda args
          (call-with-values
            (lambda () (apply g args))
            f))
        arity-mask
        #f)))

  (define (thunk? proc) (= 1 (procedure-arity-mask proc)))
  (define (all pred xs) (not (find (compose not pred) xs)))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[5]
  (define (parallel . fs)
    (let ((new-arity-mask (fold-left bitwise-and -1 (map procedure-arity-mask fs))))
      (assert (not (zero? new-arity-mask)))
      (make-wrapper-procedure
          (lambda args
              (apply values (map (lambda (f) (apply f args)) fs)))
          new-arity-mask
          #f)))

  (define (parallel-combine h . fs) (compose h (apply parallel fs)))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[6]
  (define (swap f)
    (lambda (x y) (f y x)))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[7]
  (define (partial f . args1)
    (let ((a (procedure-arity-mask f)))
        (make-wrapper-procedure
            (lambda args2
                (apply f (append args1 args2)))
            (bitwise-arithmetic-shift-right a (length args1))
            #f)))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[8]
  (define const
    (case-lambda
      ((x _) x)
      ((x) (lambda (_) x))))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[9]
  (define-syntax curry-helper
    (syntax-rules ()
      ((_ () () _ <body>) <body>)
      ((_ (<x>) () _ <body>) (lambda (<x>) <body>))
      ((_ (<xs> ... <y>) (<rest> ...) (<cases> ...) <body>)
       (curry-helper (<xs> ...) (<y> <rest> ...)
                     (<cases> ...
                      ((<xs> ... <y>)
                       (curry-helper (<rest> ...) () () <body>)))
                     <body>))
     ((_ () (<rest> ...) (<cases> ...) <body>)
      (case-lambda <cases> ...))))

  (define-syntax curry
    (syntax-rules ()
        ((_ (<xs> ...) <body> ...)
         (let ((proc (lambda (<xs> ...) <body> ...)))
           (curry-helper (<xs> ...) () () (proc <xs> ...))))))
  ; ~/~ end
  ; ~/~ begin <<docs/ch2-dsl.md#combinators>>[10]
  (define melt
    (curry (x y z) ((x z) (y z))))
  ; ~/~ end
)
; ~/~ end
