; ~/~ begin <<docs/parsing.md#scheme/parsing/lists.scm>>[init]
(library (parsing lists)
  (export list-item list-item-equal?)

  (import (rnrs (6))
          (parsing parsing))

  (define (list-item lst aux)
    (if (pair? lst)
      (values (car lst) (cdr lst) aux)
      (values (make-failure "end of list" '(list-item)) lst aux)))

  (define (list-item-equal? value)
    (satisfies list-item (lambda (x) (equal? x value))))
)
; ~/~ end
