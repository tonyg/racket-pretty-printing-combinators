#lang racket/base

(require racket/stream)

(provide append-streams
	 interleave-streams
	 merge-sorted-streams*
	 merge-sorted-streams
	 rational-stream-map
	 stream-filter-keeping-least-bad)

(define (append-streams ss)
  (if (stream-empty? ss)
      '()
      (let loop ((s (stream-first ss)))
	(if (stream-empty? s)
	    (append-streams (stream-rest ss))
	    (stream-cons (stream-first s) (loop (stream-rest s)))))))

(define (interleave-streams ss)
  (let loop ((ss (stream->list ss))
	     (ss-rev '()))
    (if (null? ss)
	(if (null? ss-rev)
	    '()
	    (loop (reverse ss-rev) '()))
	(let ((s (car ss)))
	  (if (stream-empty? s)
	      (loop (cdr ss) ss-rev)
	      (stream-cons (stream-first s) (loop (cdr ss) (cons (stream-rest s) ss-rev))))))))

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

;; Visits the cartesian product of xs and ys in the same order as a
;; duplicating traversal of the rational numbers. Lazily consumes
;; elements from each until they are exhausted. Calls f with each
;; pair. Pairs of elements from xs and ys are presented to f in some
;; sorted-with-respect-to-manhattan-distance-from-the-origin order.
(define (rational-stream-map f xs ys)
  (let walk ((flipped #f)
	     (seen-xs xs) (remaining-xs '())
	     (seen-ys '()) (remaining-ys ys))
    (cond
     [(stream-empty? remaining-ys)
      (if (stream-empty? seen-xs)
	  '()
	  (walk (not flipped)
		remaining-ys seen-ys
		(stream-cons (stream-first seen-xs) remaining-xs) (stream-rest seen-xs)))]
     [(stream-empty? remaining-xs)
      (walk (not flipped)
	    (stream-rest remaining-ys) (stream-cons (stream-first remaining-ys) seen-ys)
	    remaining-xs seen-xs)]
     [else
      (define x (stream-first remaining-xs))
      (define y (stream-first remaining-ys))
      (stream-cons (if flipped (f y x) (f x y))
		   (walk flipped
			 (stream-cons x seen-xs) (stream-rest remaining-xs)
			 (stream-cons y seen-ys) (stream-rest remaining-ys)))])))

(define (stream-filter-keeping-least-bad ? xs)
  (define filtered-xs (stream-filter ? xs))
  (if (stream-empty? filtered-xs)
      (if (stream-empty? xs)
  	  '()
  	  (list (stream-first xs))) ;; TODO: is this the least ugly/bad element to pick?
      filtered-xs))

(module+ main
  (require rackunit)

  (define-syntax-rule (noisy-stream exp ...)
    (stream (let ((v exp)) (printf "Taking ~v\n" v) v) ...))

  (for ((v (in-stream
	    (merge-sorted-streams*
	     (stream
	      (noisy-stream 1 3 5 7 9)
	      (noisy-stream 2 4 6 8 10)
	      (noisy-stream 3 6 9 12 15))
	     <))))
    (printf "FINALLY GOT ~v\n" v))

  (newline)
  (for ((v (in-stream
	    (interleave-streams (noisy-stream
				 (noisy-stream 1 2 3)
				 (noisy-stream 4 5 6 7)
				 (noisy-stream 8 9))))))
    (printf "GOT ~v\n" v))

  (check-equal? (stream->list (rational-stream-map list (stream) (stream)))
		'())

  (check-equal? (stream->list (rational-stream-map list (stream 4 5 6 7) (stream 1)))
		'((4 1) (5 1) (6 1) (7 1)))

  (check-equal? (stream->list (rational-stream-map list (stream 1) (stream 4 5 6 7)))
		'((1 4) (1 5) (1 6) (1 7)))

  (check-equal? (stream->list (rational-stream-map list (stream 4 5 6 7) (stream 1 2)))
		'((4 1) (5 1) (4 2) (5 2) (6 1) (7 1) (6 2) (7 2)))

  (check-equal? (stream->list (rational-stream-map list (stream 1 2) (stream 4 5 6 7)))
		'((1 4) (2 4) (1 5) (1 6) (2 5) (2 6) (1 7) (2 7)))

  (check-equal? (stream->list (rational-stream-map list (stream 4 5 6 7) (stream 1 2 3)))
		'((4 1) (5 1) (4 2) (4 3) (5 2) (6 1) (7 1) (6 2) (5 3) (6 3) (7 2) (7 3)))

  (check-equal? (stream->list (rational-stream-map list (stream 1 2 3) (stream 4 5 6 7)))
		'((1 4) (2 4) (1 5) (1 6) (2 5) (3 4) (3 5) (2 6) (1 7) (2 7) (3 6) (3 7)))
  )
