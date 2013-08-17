#lang racket/base

(require racket/stream)
(require racket/pretty)
(require "main.rkt")

(require (only-in "pp.rkt" doc->formats format->string))

(provide show-results)

(define (show-result d)
  (displayln (make-string (current-page-width) #\=))
  (displayln (time (doc->string d))))

(define (show-results d)
  (time (begin
	  (for ((s (in-stream (stream-map format->string (time (doc->formats d)))))
	        (limit 16))
	    (displayln (make-string (current-page-width) #\=))
	    (displayln s)))))

(module+ main
  (define (show-results* widths d)
    (newline)
    (show-results d)
    (for ((width widths))
      (newline)
      (parameterize ((current-page-width width))
	(show-results d))))

  (show-results*
   (list 40 20 10 5)
   (seq-docs "["", ""]"
	     "[ "","" ]"
	     (list "hello"
		   "world"
		   (seq-docs "{""; ""}"
			     "{ """" }"
			     (list "three1"
				   "three2"
				   "three3"
				   "three4"))
		   "fourth"
		   "fifth"
		   "sixth")))

  (show-results*
   (list 40 20 10 5)
   (sexp->doc
    '(define (take-at-most xs n)
       (cond
	[(zero? n) '()]
	[(null? xs) '()]
	[else (cons (car xs) (take-at-most (cdr xs) (- n 1)))]))
    ))

  (show-results*
   (list 40 20 10 5)
   (sexp->doc
    '(define (sexp->doc form)
       (match form
	 [(cons (? symbol? head) rest)
	  (define formatter (hash-ref (current-racket-format-map) head #f))
	  (if (procedure? formatter)
	      (formatter form)
	      (generic-sexp->doc formatter form))]
	 [(? vector?)
	  (beside "#" (sexp->doc (vector->list form)))]
	 [_
	  (with-output-to-string (lambda () (write form)))]))))

  )
