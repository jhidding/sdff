# Utility

## Receive
This is SRFI-8.

``` {.scheme title="src/utility/receive.scm"}
(library (utility receive)
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
```

## Aux keyword

``` {.scheme title="src/utility/aux-keyword.scm"}
#| Code snippet from Andy Keep |#
(library (utility aux-keyword)
  (export define-auxiliary-keyword
          define-auxiliary-keywords)

  (import (rnrs (6)))

  (define-syntax define-auxiliary-keyword
    (syntax-rules ()
      [(_ name)
       (define-syntax name
         (lambda (x)
           (syntax-violation #f "misplaced use of auxiliary keyword" x)))]))

  (define-syntax define-auxiliary-keywords
    (syntax-rules ()
      [(_ name* ...)
       (begin (define-auxiliary-keyword name*) ...)]))
)
```

## Cut

``` {.scheme title="src/utility/cut.scm"}
#| REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
 | ==========================================
 |
 | Sebastian.Egner@philips.com, 5-Jun-2002.
 | adapted from the posting by Al Petrofsky <al@petrofsky.org>
 | placed in the public domain
 |
 | The code to handle the variable argument case was originally
 | proposed by Michael Sperber and has been adapted to the new
 | syntax of the macro using an explicit rest-slot symbol. The
 | code to evaluate the non-slots for cute has been proposed by
 | Dale Jordan. The code to allow a slot for the procedure position
 | and to process the macro using an internal macro is based on
 | a suggestion by Al Petrofsky. The code found below is, with
 | exception of this header and some changes in variable names,
 | entirely written by Al Petrofsky.
 |
 | compliance:
 |   Scheme R5RS (including macros).
 |
 | loading this file into Scheme 48 0.57:
 |   ,load cut.scm
 |
 | history of this file:
 |   SE,  6-Feb-2002: initial version as 'curry' with ". <>" notation
 |   SE, 14-Feb-2002: revised for <...>
 |   SE, 27-Feb-2002: revised for 'cut'
 |   SE, 03-Jun-2002: revised for proc-slot, cute
 |   SE, 04-Jun-2002: rewritten with internal transformer (no "loop" pattern)
 |   SE, 05-Jun-2002: replace my code by Al's; substituted "constant" etc.
 |     to match the convention in the SRFI-document
 |
 | (srfi-26-internal-cut slot-names combination . se)
 |   transformer used internally
 |     slot-names  : the internal names of the slots
 |     combination : procedure being specialized, followed by its arguments
 |     se          : slots-or-exprs, the qualifiers of the macro
 |#

(library (utility cut)
  (export cut cute <> <...>)

  (import (rnrs (6))
          (utility aux-keyword))

  (define-auxiliary-keywords <> <...>)

  (define-syntax srfi-26-internal-cut
    (syntax-rules (<> <...>)

      ;; construct fixed- or variable-arity procedure:
      ;;   (begin proc) throws an error if proc is not an <expression>
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
       (lambda (slot-name ...) ((begin proc) arg ...)))
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
       (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))

      ;; process one slot-or-expr
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
       (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
       (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

  ; (srfi-26-internal-cute slot-names nse-bindings combination . se)
  ;   transformer used internally
  ;     slot-names     : the internal names of the slots
  ;     nse-bindings   : let-style bindings for the non-slot expressions.
  ;     combination    : procedure being specialized, followed by its arguments
  ;     se             : slots-or-exprs, the qualifiers of the macro

  (define-syntax srfi-26-internal-cute
    (syntax-rules (<> <...>)

      ;; If there are no slot-or-exprs to process, then:
      ;; construct a fixed-arity procedure,
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...))
       (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
      ;; or a variable-arity procedure
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...) <...>)
       (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))

      ;; otherwise, process one slot:
      ((srfi-26-internal-cute
        (slot-name ...)         nse-bindings  (position ...)   <>  . se)
       (srfi-26-internal-cute
        (slot-name ... x)       nse-bindings  (position ... x)     . se))
      ;; or one non-slot expression
      ((srfi-26-internal-cute
        slot-names              nse-bindings  (position ...)   nse . se)
       (srfi-26-internal-cute
        slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

  ; exported syntax

  (define-syntax cut
    (syntax-rules ()
      ((_ . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs))))

  (define-syntax cute
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs))))
)
```

## Yasos
Yasos is **Y**et **A**nother **S**cheme **O**bject **S**ystem. It's just awesome because its so simple.

``` {.scheme title="src/utility/yasos.scm"}
#| YASOS, Ken Dickey's 'Yet Another Scheme Object System'. The original
 | paper including most of this source is included in
 |
 |   Ken Dickey, "Scheming with Objects", AI Expert 7(10):24-33, October 1992.
 |
 | which can be found as `doc/swob.txt` in this distribution. The original
 | code is considered to be in the public domain.
 |#

(library (utility yasos)
  (export
    instance?
    define-predicate
    define-operation
    object
    object-with-ancestors
    operate-as)

  (import (rnrs (6)))

  (define-record-type instance
    (fields (immutable dispatcher)))

  (define-syntax define-operation
    (syntax-rules ()
      [(_ (<name> <inst> <arg> ...) <exp1> <exp2> ...)
       (define <name>
         (letrec [(self (lambda (<inst> <arg> ...)
                          (cond
                            [(and (instance? <inst>)
                                  ((instance-dispatcher <inst>) self))
                             => (lambda (operation) (operation <inst> <arg> ...))]
                            [else <exp1> <exp2> ...])))]
           self))]

      [(_ (<name> <inst> <arg> ...)) ; no body
       (define-operation (<name> <inst> <arg> ...)
         (error '<name> "Operation not handled: {}" <inst>))]
    ))

  (define-syntax define-predicate
    (syntax-rules ()
      [(_ <name>) (define-operation (<name> obj) #f)]))

  (define-syntax object
    (syntax-rules ()
      [(_ ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...)
       (let [(table (list
                      (cons <name>
                        (lambda (<self> <arg> ...) <exp1> <exp2> ...))
                      ...))]
         (make-instance
           (lambda (operation)
             (cond
               [(assq operation table) => cdr]
               [else #f]))))]))

  (define-syntax object-with-ancestors
    (syntax-rules ()
      [(_ ([<ancestor1> <init1>] ...) <operation> ...)
       (let ([<ancestor1> <init1>] ...)
         (let ([child (object <operation> ...)])
           (make-instance
             (lambda (operation)
               (or ((instance-dispatcher child) operation)
                   ((instance-dispatcher <ancestor1>) operation)
                   ...)))))]))

  (define-syntax operate-as
    (syntax-rules ()
      [(_ <component> <operation> <composit> <arg> ...)
       (((instance-dispatcher <component>) <operation>) <composit> <arg> ...)]))
)
```

## PMatch

``` {.scheme title="src/utility/pmatch.scm"}
(library (utility pmatch)
  (export pmatch)
  (import (rnrs (6)))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ v (e ...) ...)
     (pmatch-aux #f v (e ...) ...))
    ((_ v name (e ...) ...)
     (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch-aux
  (syntax-rules (else guard)
    ((_ name (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch-aux name v cs ...)))
    ((_ name v)
     (begin
       (if 'name
           (begin (display "pmatch ") (display 'name) (display " failed") (newline)
                  (display v) (newline))
           (begin (display "pmatch failed") (newline) (display v) (newline)))
       (error 'pmatch "match failed")))
    ((_ name v (else e0 e ...)) (begin e0 e ...))
    ((_ name v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ name v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (? comma unquote)
    ((_ v ? kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
;   ((_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
         (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
)
```

### tests

``` {.scheme title="test/test-pmatch.scm}
(import (rnrs (6))
        (testing assertions)
        (utility pmatch))

(define (test-pmatch)
  (assert-equal
   (pmatch '(1 2 3)
     ((,a . ,b) b))
   '(2 3))

  (assert-equal
    (pmatch '(define (hello) 'world)
      ((define (,name . ,args) . ,body)
       name)
      (else #f))
    'hello)
)
```

## algorithms

``` {.scheme title="src/utility/algorithms.scm"}
(library (utility algorithms)
  (export append-reverse append-map string-join unfold range iterate-n find-index
          split-at group-by combinations unique cartesian-product)
  (import (rnrs (6))
          (utility receive)
          (utility cut))

  (define (append-reverse rev-head tail)
    (if (null? rev-head)
        tail
        (append-reverse
         (cdr rev-head)
         (cons (car rev-head) tail))))

  (define (append-map f . args)
    (apply append (apply map f args)))

  (define (iterate-n f x n)
    (if (zero? n)
      x
      (iterate-n f (f x) (- n 1))))

  (define range
    (case-lambda
      ((n)   (range 0 n))
      ((a b) (do ((i a (+ i 1))
                  (r '() (cons i r)))
                 ((>= i b) (reverse r))))))

  (define (find-index elem lst)
    (let loop ((lst lst)
               (count 0))
      (cond
        ((null? lst)          #f)
        ((eq? elem (car lst)) count)
        (else                 (loop (cdr lst) (+ count 1))))))

  (define unfold
    (case-lambda
      ((p f g seed) (unfold p f g seed (lambda (x) '())))
      ((p f g seed tail-gen)
       (do ((x seed (g x))
            (result '() (cons (f x) result)))
           ((p x) (cons (tail-gen x) (reverse result)))))))
  
  (define (string-join sep lst)
    (if (null? lst)
      ""
      (do ((result (car lst) (string-append result sep (car rest)))
           (rest   (cdr lst) (cdr rest)))
          ((null? rest) result))))

  #| Splits a list into a reverse-head and tail portions where the first
   | element of tail is the first element for which pred returns #t.
   |#
  (define (split-at pred lst)
    (let loop ((head '())
               (tail lst))
      (if (or (null? tail) (pred (car tail)))
        (values head tail)
        (loop (cons (car tail) head) (cdr tail)))))

  #| Creates an associative list from a given list and a key function. The
   | argument `key-fn` should be a function mapping elements to their keys,
   | while `equiv` is a function testing the keys for equivalence. This preserves
   | order of the input list, meaning that the expressions:
   | - (unique (map key-fn lst))
   | - (map car (group-by key-fn equiv lst))
   | evaluate to the same result, and elements within the groups appear in the
   | same order as they were in the input list.
   |#
  (define (group-by key-fn equiv lst)
    (let ((update-bin (lambda (elem alst)
                        (receive (head tail) (split-at (lambda (p) (equiv (car p) (key-fn elem))) alst)
                          (if (null? tail)
                            (append-reverse head (list (list (key-fn elem) elem)))
                            (append-reverse head (cons (cons* (caar tail) elem (cdar tail)) (cdr tail)))))))
          (reverse-alist (lambda (alst)
                           (map (lambda (a) (cons (car a) (reverse (cdr a)))) alst))))
      (let loop ((lst lst)
                 (result '()))
        (if (null? lst)
          (reverse-alist result)
          (loop (cdr lst) (update-bin (car lst) result))))))

  (define (combinations m lst)
    (cond
      ((= m 0)     '(()))
      ((null? lst) '())
      (else (append
              (map (lambda (y) (cons (car lst) y))
                   (combinations (- m 1) (cdr lst)))
              (combinations m (cdr lst))))))

  (define (unique equiv? lst)
    (if (null? lst)
      '()
      (let loop ((lst (cdr lst))
                 (res (cons (car lst) '())))
        (cond
          ((null? lst)                  (reverse res))
          ((equiv? (car lst) (car res)) (loop (cdr lst) res))
          (else                         (loop (cdr lst) (cons (car lst) res)))))))

  (define (cartesian-product f first . rest)
    (let* ((curry1 (lambda (f) (lambda (a) (lambda args (apply f a args)))))
           (ap     (lambda (f a)
                     (append-map (lambda (f*) (map (curry1 f*) a)) f))))
      (map (lambda (f) (f))
           (fold-left ap (map (curry1 f) first) rest))))
)
```

``` {.scheme title="test/test-utility.scm"}
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
```

## Formatted printing

``` {.scheme title="src/format/format.scm"}
(library (format format)
  (export format print println formatter)
  (import (rnrs (6))
          (parsing parsing)
          (monads monads))

  (define-record-type spec
    (fields fill align sign sharp zero width precision type))

  (define identifier
    (seq <parsing>
         (satisfies item char-alphabetic?)
         (many-char* (satisfies item 
                                (lambda (c)
                                        (or (char-alphabetic? c)
                                            (char-numeric? c)
                                            ((char-in "_-") c)))))
         (flush)))

  (define argument
    (choice integer identifier))

  (define parameter
    (seq <parsing>
         (x <- argument)
         (literal "$")
         (parsing-return x)))

  (define count
    (choice parameter integer))

  (define spec-parser
    (seq <parsing>
         (fill      <- (optional (look-ahead
                                  (one item)
                                  (one (char= #\< #\^ #\>)))))
         (align     <- (optional (one (char= #\< #\^ #\>))))
         (sign      <- (optional (one (char= #\+ #\-))))
         (sharp     <- (optional (one (char= #\#))))
         (zero      <- (optional (one (char= #\0))))
         (width     <- (optional (choice count (one (char= #\*)))))
         (precision <- (optional (seq <parsing> (one (char= #\.)) count)))
         (type      <- (optional identifier))
         (parsing-return
           (make-spec fill align sign sharp zero width precision type))))

  (define escape
    (choice (seq <parsing>
                 (literal "{{")
                 (parsing-return "{"))
            (seq <parsing>
                 (literal "}}")
                 (parsing-return "}"))))

  (define clause
    (seq <parsing>
         (literal "{")
         (arg <- (optional argument))
         (fmt <- (optional (seq <parsing>
                                (literal ":")
                                spec-parser)))
         (literal "}")
         (parsing-return (cons arg fmt))))

  (define text
    (some-char (char!= #\{ #\})))

  (define format-string-parser
    (many (choice text escape clause)))

  (define (parse-format-string str)
    (parse-string
     format-string-parser
     str))

  (define (format-value spec value port)
    ;;(display "formating: ") (write value) (display ", type: ")
    ;;(write (if (nothing? spec) "<nothing>" (spec-type spec))) (newline)
    (cond
     ((nothing? spec)            (display value port))
     ((equal? (spec-type spec) "s") (write value port))
     ((equal? (spec-type spec) "f") (write (inexact value) port))
     (else (display value port))))

  (define (format-arguments fmt-lst args port)
    (let loop ((iter-args args)
               (items     fmt-lst))
      (cond
       ((null? items)         #f)

       ((string? (car items))
        (display (car items) port)
        (loop iter-args (cdr items)))

       ((pair?   (car items))
        (cond
         ((nothing? (caar items))
          (when (null? iter-args)
            (raise "format: not enough arguments"))
          (format-value (cdar items)
                        (car iter-args)
                        port)
          (loop (cdr iter-args)
                (cdr items)))

         ((number? (caar items))
          (when (>= (caar items) (length args))
            (raise "format: not enough arguments"))
          (format-value (cdar items)
                        (list-ref args (caar items))
                        port)
          (loop iter-args
                (cdr items)))

         ((string? (caar items))
          (raise "format: named arguments not supported"))

         (else (raise "format: sanity failure"))))

       (else (raise "format: sanity failure")))))

  (define (formatter str)
    (let ((fmt-lst (parse-format-string str)))
      (lambda args
        (let-values (((output g) (open-string-output-port)))
          (format-arguments fmt-lst args output)
          (g)))))

  (define (format str . args)
    (apply (formatter str) args))

  (define (print str . args)
    (display (apply (formatter str) args)))

  (define (println str . args)
    (apply print str args) (newline))
)
```
