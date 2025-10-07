# Domain Specific Languages

``` {.scheme .repl #scheme-repl}
;| session: .entangled/repl-session/scheme-ch2.json
(import
    (rnrs)
    (combinators)
    (invertibles))
```

``` {.python file=python/sdff/__init__.py}
"""
Main module for the Software Design for Flexibility book.
"""
```

``` {.python .repl #python-repl}
#| session: .entangled/repl-session/python-ch2.json
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

In Python we can write similar things:

``` {.python file=python/sdff/combinators.py}
from collections.abc import Callable
from functools import reduce

def compose_2[T, U, V](f: Callable[[U], V], g: Callable[[T], U]) -> Callable[[T], V]:
    """Composes two function of a single argumeent."""
    def composed(a: T) -> V:
        return f(g(a))

    return composed

def compose(*args):
    return reduce(compose_2, args)

<<combinators-python>>
```

Now, Python does not have the similar distinction that Scheme has when it comes to multi-valued returns. We can however write another combinator to transform a function of many arguments into a functionn accepting a single tuple:

``` {.python #combinators-python}
def splat[*Ts, U](f: Callable[[*Ts], U]) -> Callable[[tuple[*Ts]],U]:
    return lambda a: f(*a)
```

Example:

=== "Scheme"

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

=== "Python"

    ``` {.python #combinators-python}
    def vmap(f):
        def vmapped(*args):
            return map(f, args)
        return vmapped
    ```

    ``` {.python .repl #python-repl}
    from sdff.combinators import compose, splat, vmap
    from functools import partial

    def sqr(x):
        return x*x
    dot = compose(sum, partial(map, sqr))
    dot([1, 2, 3])
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

We can try this on a multiply-add routine:

``` {.scheme .repl #scheme-repl}
(define mac (curry (a b c) (+ a (* b c))))
```

``` {.scheme .repl #scheme-repl}
(mac 3 4 5)
```

``` {.scheme .repl #scheme-repl}
((mac 3) 4 5)
```

``` {.scheme .repl #scheme-repl}
((mac 3 4) 5)
```

Now, we can simply write:

``` {.scheme #combinators}
(define melt
  (curry (x y z) ((x z) (y z))))
```

### Library

We make it so that we can add to our combinators library when needed.

``` {.scheme file=scheme/combinators.scm}
(library (combinators)
  (export
      <<combinators-export>>
  )
  (import (rnrs)
      <<combinators-import>>
  )
  <<combinators>>
)
```

``` {.scheme #combinators-export}
compose identity iterate parallel parallel-combine vmap const melt curry curry-helper
```

``` {.scheme #combinators-import}
(only (chezscheme) procedure-arity-mask make-wrapper-procedure)
```

## Regular Expressions

We'll skip this section. It's a nice example of a DSL, and we should note the reasons why rephrasing regexes into this form can be useful:

- always look at the primitives, the means of abstraction and means of combination
- is the system closed under the means of combination?

It turns out that regexes are not composable without rewriting.
Why I'm skipping this: it looks like a lot of work with not much learned (yes regexes are terrible we all know). I prefer using a parser-combinator DSL where we can use regexes as primitives. Regexes are well known and can be used for configuring an application.

## Wrappers

We get an example around the ideal gas law.

``` {.scheme}
(define (gas-law-volume pressure temperature amount)
    (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)  ; J / (K mol)

(define (sphere-radius volume)
    (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))
```

These equations assume we use standard SI units. We can write additional functions to convert between units.

``` {.scheme}
(define fahrenheit-to-celcius
    (make-invertible
        (lambda (f) (* 5/9 (- f 32)))
        (lambda (c) (+ (* c 9/5) 32))))

(define celcius-to-kelvin
    (let ((zero-celcius 273.15))  ; K
        (make-invertible
            (lambda (c) (+ c zero-celcius))
            (lambda (k) (- k zero-celcius)))))
```

This uses non-standard Scheme code, and it is not very nice at all. We can abuse Chez Scheme's `wrapper-procedure` mechanism.

``` {.scheme file=scheme/invertibles.scm}
(library (invertibles)
  (export make-invertible invertible? invert)
  (import (rnrs)
          (only (chezscheme)
              make-wrapper-procedure
              wrapper-procedure?
              wrapper-procedure-procedure
              wrapper-procedure-data))

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
```

The way that physical units are dealt with in other systems is either through the type system (Julia's `Unitful.jl` or similar systems in C++), or as in Python's [Pint](https://pint.readthedocs.io), where values are combined with units to create a quantity.

Maybe, some day I'll have the patience to implement this feature that already exists in most prevailing computation languages (and with much more convenient syntax too).

``` {.scheme file=scheme/units.scm}
(library (units)
  (import (rnrs)
          (combinators)
          (invertibles))
)
```

## Checkers

This seems a little bit more fun. We build an engine that can check legality of checkers moves.

``` {.scheme file=scheme/checkers.scm}
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
```
