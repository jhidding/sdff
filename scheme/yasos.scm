; ~/~ begin <<docs/utility.md#scheme/yasos.scm>>[init]
#| YASOS, Ken Dickey's 'Yet Another Scheme Object System'. The original
 | paper including most of this source is included in
 |
 |   Ken Dickey, "Scheming with Objects", AI Expert 7(10):24-33, October 1992.
 |
 | which can be found as `doc/swob.txt` in this distribution. The original
 | code is considered to be in the public domain.
 |#

(library (yasos)
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
; ~/~ end
