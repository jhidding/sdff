; ~/~ begin <<docs/ch2-4-checkers.md#scheme/checkers.scm>>[init]
(library (checkers)
  (export make-board board->rtf current-pieces is-position-on-board? board-get)
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

  (define (piece->rtf p)
    (if p
      `((fg ,(piece-colour p))
        ,(if (eq? (piece-type p) 'single)
           "⛂ "
           "⛃ "))
      "  "))

  (define (board->rtf b)
    (define cel->rtf (curry (r c)
      `((bg ,(if (odd? (+ r c)) 'magenta 'blue))
        ,(piece->rtf (board-get b (list r c))))))
    (define (row->rtf r)
      (append (map  (cel->rtf r) (unit-range 0 (board-size b))) '(reset newline)))
    (apply append (map row->rtf (unit-range 0 (board-size b)))))

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
    (and
      (is-position-on-board? b pos)
      (vector-ref
        (vector-ref (board-fields b) (car pos))
        (cadr pos))))

  (define (position-info pos board)
    (let ((player (board-turn board))
	  (status (board-get board pos)))
      (cond
	((not status) 'unoccupied)
	((eq? (piece-colour status) player) 'occupied-by-self)
	(else 'occupied-by-opponent))))
)
; ~/~ end
