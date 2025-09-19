# Domain Specific Languages

## Combinators

We meet our first bit of Scheme code in this chapter, and it is the `compose` function!

```scheme
(define (compose f g)
  (lambda args (f (apply g args))))
```

We can immediately improve on this by matching the arity of `f` with the multi-valued output of `g`. We can write a binomial form:

```scheme
(define (compose-2 f g)
  (lambda args
    (call-with-values
      (lambda () (apply g args))
      f)))
```

and then use `fold-right` to extend this to chain any number of functions. First defining the `identity` function:

```scheme
;| id: functional
(define identity values)
```

```scheme
;| id: functional
(define (compose . fs)
  (fold-right compose-2 identity fs))
```

We now continue to write the `iterate` function:

```scheme
;| id: functional
(define (iterate n)
  (lambda (f)
    (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f)))))
```

Note however that the version presented in the book is not standard Scheme syntax. Next, we can define the `parallel-combine` combinator. This version works with any number of arguments.

```scheme
(define (parallel-combine h . fs)
  (lambda args
      (apply h (map (lambda (f) (apply f args)) fs))))
```

## Exercise 2.1

Here I'll cheat and use the Chez Scheme implementation of the `get-arity` and `set-arity` functions. Basically because I don't like the hacky implementation given in the book.

```scheme
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
```

## Exercise 2.2

For `compose` it is too strict to only allow strict arity procedures. We can copy the `arity-mask` from the last procedure in the composition.

In the case of `parallel-combine`, we should take the bitwise `and` of all the function arguments `fs`. If this is zero, then all functions in `fs` should be thunks. If the combined arity mask is non-zero, the result is the largest common arity accepted by all of the functions in `fs`. We see how smart the choice is to store the arity in a bit-mask.

```scheme
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
```

```scheme
;| file: scheme/functional.scm
(define-library (functional)
  (export compose identity iterate parallel-combine)
  (import (rnrs)
          (only (chezscheme) procedure-arity-mask make-wrapper-procedure))

  <<functional>>
)
```
