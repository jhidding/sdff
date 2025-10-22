# Rich Text Formatting

``` {.scheme file=scheme/rtf.scm}
(library (rtf)
  (export rtf->string)
  (import (rnrs)
	  (std match)
	  (std format)
	  (monads syntax)
	  (monads maybe))

  (define ansi-colours
    '((black   . 0)
      (red     . 1)
      (green   . 2)
      (yellow  . 3)
      (blue    . 4)
      (magenta . 5)
      (cyan    . 6)
      (white   . 7)))

  (define (massq obj alist)
    (maybe-bind (or (assq obj alist) *nothing*) cdr))

  (define (ansi-colour-string selector base sym)
    (seq <maybe>
      (n <- (massq sym ansi-colours))
      (format #f "\x1b;[~s;5;~sm" selector (+ base n))))

  (define (ansi-fg sym) (ansi-colour-string 30 sym))
  (define (ansi-bg sym) (ansi-colour-string 40 sym))

  (define (rtf->string rtf)
    (match rtf
      (newline "\n")
      (reset   "\x1b;[m")
      (bold    "\x1b;[1m")
      ((fg ,c)        (ansi-colour-string 38 0 c))
      ((bg ,c)        (ansi-colour-string 48 0 c))
      ((fg bright ,c) (ansi-colour-string 38 8 c))
      ((bg bright ,c) (ansi-colour-string 48 8 c))
      ((,x ...) (apply string-append (map rtf->string x)))
      (,item   (guard (string? item)) item)))
)
```
