#lang racket/base

(require racket/stream)

(provide merge-sorted-streams*
	 merge-sorted-streams)

(define (merge-sorted-streams* ls <)
  (stream-fold (lambda (x y) (merge-sorted-streams x y <)) '() ls))

(define (merge-sorted-streams xs ys <)
  (cond
   [(stream-empty? xs) ys]
   [(stream-empty? ys) xs]
   [else (define x (stream-first xs))
	 (define y (stream-first ys))
	 (if (< x y)
	     (stream-cons x (merge-sorted-streams (stream-rest xs) ys <))
	     (stream-cons y (merge-sorted-streams xs (stream-rest ys) <)))]))

(module+ main
  (define-syntax-rule (noisy-stream exp ...)
    (stream (let ((v exp)) (printf "Taking ~v\n" v) v) ...))

  (for ((v (in-stream
	    (merge-sorted-streams*
	     (stream
	      (noisy-stream 1 3 5 7 9)
	      (noisy-stream 2 4 6 8 10)
	      (noisy-stream 3 6 9 12 15))
	     <))))
    (printf "FINALLY GOT ~v\n" v)))
