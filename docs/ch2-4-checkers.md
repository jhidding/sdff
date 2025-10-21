# Checkers

This seems a little bit more fun. We build an engine that can check legality of checkers moves.

``` {.scheme .repl #checkers-repl}
;| session: .entangled/repl-session/checkers.json
(import (rnrs)
        (combinators)
        (utility)
        (checkers)
        )
```

## Utility functions

``` {.scheme file=scheme/utility.scm}
(library (utility)
  (export range unit-range cartesian-product)
  (import (rnrs) (combinators))

  <<scheme-utility>>
)
```

We'll need some utility functions. The `range` and `unit-range` functions, 

``` {.scheme .repl #checkers-repl}
(range 10 40 5)
```

``` {.scheme .repl #checkers-repl}
(unit-range 30 40)
```

``` {.scheme #scheme-utility}
(define (range a b step)
   (do ((x a (+ x step))
        (r '() (cons x r)))
       ((>= x b) (reverse r))))

(define (unit-range a b) (range a b 1))
```

And the `cartesian-product` functionn

``` {.scheme .repl #checkers-repl}
(cartesian-product '(1 2 3) '(a b c) '(#f #t))
```

``` {.scheme #scheme-utility}
(define cartesian-product-2 (curry (f as bs)
  (apply append (map (lambda (a) (map (lambda (b) (f a b)) bs)) as))))

(define (cartesian-product . args)
  (fold-right (cartesian-product-2 cons) '(()) args))
```

## Domain model

Next we need to implement the *domain model*. We are given a list of functions to implement.

``` {.scheme .repl #checkers-repl}

```

``` {.scheme file=scheme/checkers.scm}
(library (checkers)
  (export range cartesian-product make-board current-pieces)
  (import (rnrs)
          (combinators)
          (utility))

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
      (lambda (p) (eq? (board-turn b) (try piece-colour (board-get b p))))
      (used-fields b)))

  (define (is-position-on-board? b pos)
    (let ((row (car pos))
          (col (cadr pos)))
      (and (<= 0 row) (< row (board-size b))
           (<= 0 col) (< col (board-size b))
           (odd? (+ col row)))))

  (define (board-get b pos)
    (assert (is-position-on-board? b pos))
    (vector-ref
      (vector-ref (board-fields b) (car pos))
      (cadr pos)))
)
```
