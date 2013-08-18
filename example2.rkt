#lang racket/base

(require "main.rkt")

(current-page-width 110)
(with-input-from-file "pp-racket.rkt"
  (lambda ()
    (read-line) ;; skip #lang line
    (let loop ()
      (define s (read))
      (if (eof-object? s)
	  (void)
	  (begin (displayln (sexp->string s))
		 (newline)
		 (loop))))))
