; ~/~ begin <<docs/utility.md#scheme/std/receive.scm>>[init]
(library (std receive)
  (export receive)
  (import (rnrs (6)))

  ;;; (srfi :8 receive)
  (define-syntax receive
    (syntax-rules ()
      ((_ <formals> <expr> <body> ...)
       (call-with-values
         (lambda () <expr>)
         (lambda <formals> <body> ...)))))
)
; ~/~ end
