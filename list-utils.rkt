#lang racket/base

(provide take-at-most
	 drop-at-most)

(define (take-at-most xs n)
  (cond
   [(zero? n) '()]
   [(null? xs) '()]
   [else (cons (car xs) (take-at-most (cdr xs) (- n 1)))]))

(define (drop-at-most xs n)
  (cond
   [(zero? n) xs]
   [(null? xs) xs]
   [else (drop-at-most (cdr xs) (- n 1))]))
