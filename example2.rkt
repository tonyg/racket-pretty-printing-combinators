#lang racket/base

(require racket/pretty)
(require "main.rkt")

(define (pp-racket-source)
  (read-line) ;; skip #lang line
  (let loop ()
    (define s (read))
    (if (eof-object? s)
	(void)
	(begin (pretty-write s)
	       (displayln (sexp->string s))
	       (newline)
	       (loop)))))

(current-page-width 80)
(with-input-from-file "stream-utils.rkt" pp-racket-source)
(with-input-from-file "pp.rkt" pp-racket-source)
(with-input-from-file "pp-racket.rkt" pp-racket-source)
