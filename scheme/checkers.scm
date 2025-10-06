; ~/~ begin <<docs/ch2-dsl.md#scheme/checkers.scm>>[init]
(library (checkers)
  (export range cartesian-product make-board current-pieces)
  (import (rnrs)
          (combinators))

  (define (range a b step)
     (do ((x a (+ x step))
          (r '() (cons x r)))
         ((>= x b) (reverse r))))

  (define (unit-range a b) (range a b 1))

  (define cartesian-product-2 (curry (f as bs)
    (apply append (map (lambda (a) (map (lambda (b) (f a b)) bs)) as))))

  (define (cartesian-product . args)
    (fold-right (cartesian-product-2 cons) '(()) args))

  (define try (curry (f x)
      (if x (f x) x)))

  (define-record-type board
    (fields size turn fields)
    (protocol
      (lambda (new)
        (lambda (size)
          (let ((fields (apply vector (map (lambda (row)
                                             (apply vector (map (start-position size row)
                                                                (unit-range 0 size))))
                                           (unit-range 0 size)))))
            (new size 'white fields))))))

  (define-record-type piece
    (fields colour type))

  (define black-single (make-piece 'black 'single))
  (define white-single (make-piece 'white 'single))
  (define black-crowned (make-piece 'black 'crowned))
  (define white-crowned (make-piece 'white 'crowned))

  (define start-position (curry (size row col)
    (cond
      ((even? (+ row col)) #f)
      ((< row (- (/ size 2) 1)) white-single)
      ((>= row (+ (/ size 2) 1)) black-single)
      (else #f))))

  (define (all-fields size)
    (cartesian-product (unit-range 0 size) (unit-range 0 size)))

  (define (used-fields b)
    (filter
        (lambda (p) (odd? (+ (car p) (cadr p))))
        (all-fields (board-size b))))

  (define (current-pieces b)
    (filter
      (lambda (p) (eq? (board-turn b) (try piece-colour (get-piece b p))))
      (used-fields b)))

  (define (position-on-board? b pos)
    (let ((row (car pos))
          (col (cadr pos)))
      (and (<= 0 row) (< row (board-size b))
           (<= 0 col) (< col (board-size b))
           (odd? (+ col row)))))

  (define (board-get b pos)
    (assert (position-on-board? b pos))
    (vector-ref
      (vector-ref (board-fields b) (car pos))
      (cadr pos)))
)
; ~/~ end
