# Domain Specific Languages

``` {.scheme .repl #scheme-repl}
;| session: .entangled/repl-session/scheme-ch2.json
(import
    (rnrs)
    (combinators))
```

## Combinators

We meet our first bit of Scheme code in this chapter, and it is the `compose` function!

``` {.scheme}
(define (compose f g)
  (lambda args (f (apply g args))))
```

We can immediately improve on this by matching the arity of `f` with the multi-valued output of `g`. We can write a binomial form:

``` {.scheme}
(define (compose-2 f g)
  (lambda args
    (call-with-values
      (lambda () (apply g args))
      f)))
```

and then use `fold-right` to extend this to chain any number of functions. First defining the `identity` function:

``` {.scheme #combinators}
(define identity values)
```

``` {.scheme #combinators}
(define (compose . fs)
  (fold-right compose-2 identity fs))
```

Example:

``` {.scheme #combinators}
(define (vmap f)
    (lambda args
        (apply values (map f args))))
```

``` {.scheme .repl #scheme-repl}
(define (square x) (* x x))
```

``` {.scheme .repl #scheme-repl}
(define dot (compose + (vmap square)))
```

``` {.scheme .repl #scheme-repl}
(dot 1 2 3)
```

We now continue to write the `iterate` function:

``` {.scheme #combinators}
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

### Exercise 2.1

Here I'll cheat and use the Chez Scheme implementation of the `get-arity` and `set-arity` functions. Basically because I don't like the hacky implementation given in the book.

``` {.scheme #combinators}
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
```

### Exercise 2.2

For `compose` it is too strict to only allow strict arity procedures. We can copy the `arity-mask` from the last procedure in the composition.

In the case of `parallel-combine`, we should take the bitwise `and` of all the function arguments `fs`. If this is zero, then all functions in `fs` should be thunks. If the combined arity mask is non-zero, the result is the largest common arity accepted by all of the functions in `fs`. We see how smart the choice is to store the arity in a bit-mask.

``` {.scheme}
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

### Exercise 2.3

Now we split the `parallel-combine` routine in two parts.

``` {.scheme #combinators}
(define (parallel . fs)
  (let ((new-arity-mask (fold-left bitwise-and -1 (map procedure-arity-mask fs))))
    (assert (not (zero? new-arity-mask)))
    (make-wrapper-procedure
        (lambda args
            (apply values (map (lambda (f) (apply f args)) fs)))
        new-arity-mask
        #f)))

(define (parallel-combine h . fs) (compose h (apply parallel fs)))
```

The following computes $2^4$, $2 \times 4$, $2 / 4$, $2 + 4$ and $2 - 4$ all in one go!

``` {.scheme .repl #scheme-repl}
((parallel-combine list expt * / + -) 2 4)
```

### Rest of the section

I'm extremely critical of this kind of programming. Yes, you can do all this, but should you want to? Functions of many more arguments should use keyword arguments to pass along values. I can't imagine a system where using positional argument permutation leads to readable code. There's a few exceptions which I'll include in my `combinators` library.

Swapping arguments of a binomial:

``` {.scheme #combinators}
(define (swap f)
  (lambda (x y) (f y x)))
```

Partial application as a poor-man's currying:

``` {.scheme #combinators}
(define (partial f . args1)
  (let ((a (procedure-arity-mask f)))
      (make-wrapper-procedure
          (lambda args2
              (apply f (append args1 args2)))
          (bitwise-arithmetic-shift-right a (length args1))
          #f)))
```

In the case for partial application it is often more efficient to use Scheme's `cut` routine from SRFI 26, which uses macros.

For more complicated argument shuffles, it is often most readable to write a `lambda` function. The case being that using combinators is functionally equivalent to lambda calculus. A well known set of combinators that are Turing complete are the [`S`, `K` and `I` combinators](https://en.wikipedia.org/wiki/SKI_combinator_calculus). We already have `I`, being the `identity` function. `K` is the following `const` function (here manually Curried):

``` {.scheme #combinators}
(define const
  (case-lambda
    ((x _) x)
    ((x) (lambda (_) x))))
```

Lastly, the `S` combinator stands for `melt`:

```scheme
(define melt
  (case-lambda
    ((x y z) ((x z) (y z)))
    ((x y) (lambda (z) ((x z) (y z))))
    ((x) (case-lambda ((y z) ((x z) (y z)))
                      ((y) (lambda (z) ((x z) (y z))))))))
```

Maybe I should write a macro for auto-currying functions.

``` {.scheme #combinators}
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
```

Now, we can simply write:

``` {.scheme #combinators}
(define melt
  (curry (x y z) ((x z) (y z))))
```

``` {.scheme file=scheme/combinators.scm}
(library (combinators)
  (export compose identity iterate parallel vmap const melt curry curry-helper)
  (import (rnrs)
          (only (chezscheme) trace-define-syntax procedure-arity-mask make-wrapper-procedure))

  <<combinators>>
)
```
