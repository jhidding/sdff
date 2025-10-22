; ~/~ begin <<docs/utility.md#test/test-utility.scm>>[init]
(import (rnrs (6))
        (testing assertions)
        (utility algorithms))

(define (test-find-index)
  (let ((a '(1 2 3 4 5 6 a)))
    (assert-equal (find-index 3 a) 2)
    (assert-equal (find-index 'a a) 6)
    (assert-equal (find-index 'b a) #f)))

(define (test-group-by)
  (assert-equal (group-by odd? eq? '(1 2 3 4 5 6))
                '((#t 1 3 5) (#f 2 4 6)))
  (assert-equal (group-by (lambda (x) (mod x 3)) eq? '(1 2 3 4 5 6))
                '((1 1 4) (2 2 5) (0 3 6))))

(define (test-cartesian-product)
  (assert-equal (cartesian-product cons '(1 2 3) '(a b c))
                '((1 . a) (1 . b) (1 . c)
                  (2 . a) (2 . b) (2 . c)
                  (3 . a) (3 . b) (3 . c))))
; ~/~ end
