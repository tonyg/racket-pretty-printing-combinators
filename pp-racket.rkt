#lang racket/base
;; Pretty-print Racket code

(require racket/match)
(require (only-in racket/port with-output-to-string))
(require "pp.rkt")
(require "list-utils.rkt")

(provide ;; Interface
         current-racket-format-map
	 current-big-indent
	 current-normal-indent
	 sexp->doc
	 sexp->string

	 ;; Lower-level formatting utilities
	 generic-sexp->doc
	 let-sexp->doc
	 cond-like-sexp->doc
	 case-like-sexp->doc
	 generic-quoted-sexp->doc
	 hash-like-sexp->doc
	 )

;;---------------------------------------------------------------------------

(define (height-one? e)
  (= (element-height e) 1))

(define (generic-sexp->doc special-argument-count form #:parens [parens 'round])
  (define-values (LP RP) (match parens
			   ['round (values "(" ")")]
			   ['square (values "[" "]")]
			   ['curly (values "{" "}")]
			   [(cons l r) (values l r)]))

  (match-define (cons head args) form)
  (define head-doc (sexp->doc head))
  (define arg-docs
    (if (list? args)
	(map sexp->doc args)
	(list "." (sexp->doc args))))

  (define (layout-variations distance arg-docs)
    (if (= (length arg-docs) 1)
	(choice (reject-unless height-one?
			       (beside/space head-doc (car arg-docs)))
		(above head-doc (indent distance (apply above* arg-docs))))
	(choice (reject-unless height-one?
			       (apply beside*/space (cons head-doc arg-docs)))
		(beside/space head-doc (apply above* arg-docs))
		(above head-doc (indent distance (apply above* arg-docs))))))

  (beside* LP
	   (cond
	    [(not special-argument-count)
	     (layout-variations 0 arg-docs)]
	    [(zero? special-argument-count)
	     (layout-variations (current-normal-indent) arg-docs)]
	    [(positive? special-argument-count)
	     (define special-arg-docs (take-at-most arg-docs special-argument-count))
	     (define ordinary-arg-docs (drop-at-most arg-docs special-argument-count))
	     (choice (reject-unless height-one? (apply beside*/space (cons head-doc arg-docs)))
		     (above (layout-variations (current-big-indent) special-arg-docs)
			    (indent (current-normal-indent) (apply above* ordinary-arg-docs))))])
	   RP))

(define (let-sexp->doc form)
  (match form
    [`(let ,(? symbol?) ,_ ...) (generic-sexp->doc 2 form)]
    [_ (generic-sexp->doc 1 form)]))

(define (cond-like-clause-sexp->doc form)
  (match-define (cons test body-forms) form)
  (define test-doc (sexp->doc test))
  (define body-docs (map sexp->doc body-forms))
  (define vertical (beside* "[" (above test-doc (apply above* body-docs)) "]"))
  (match body-docs
    [(list body-doc)
     (choice (reject-unless height-one? (beside* "[" test-doc " " body-doc "]"))
	     vertical)]
    [_ vertical]))

(define (case-like-sexp->doc form)
  (match form
    [`(,head ,value [,test ,body ...] ...)
     (define head-doc (sexp->doc head))
     (define value-doc (sexp->doc value))
     (beside* "("
	      (above
	       (choice (beside/space head-doc value-doc)
		       (above head-doc
			      (indent (current-big-indent) value-doc)))
	       (indent (current-normal-indent)
		       (apply above*
			      (map (lambda (test body-forms)
				     (cond-like-clause-sexp->doc (cons test body-forms)))
				   test
				   body))))
	      ")")]
    [_ (generic-sexp->doc 1 form)]))

(define (cond-like-sexp->doc form)
  (match form
    [`(,head [,test ,body ...] ...)
     (define head-doc (sexp->doc head))
     (define clauses-doc (apply above*
				(map (lambda (test body-forms)
				       (cond-like-clause-sexp->doc (cons test body-forms)))
				     test
				     body)))
     (beside* "("
	      (choice (beside/space head-doc clauses-doc)
		      (above head-doc
			     (indent (current-normal-indent) clauses-doc)))
	      ")")]
    [_ (generic-sexp->doc 0 form)]))

(define ((generic-quoted-sexp->doc prefix) form)
  (match form
    [(list _ body)
     (beside prefix (sexp->doc body))]
    [_ (generic-sexp->doc 1 form)]))

(define (hash-like-sexp->doc form)
  (match form
    [(cons head items)
     (define item-docs (map sexp->doc items))
     (beside* "("
	      (sexp->doc head)
	      " "
	      (choice
	       (let walk ((item-docs item-docs))
		 (match item-docs
		   ['()
		    (empty-doc)]
		   [(list last)
		    last]
		   [(list* first second rest)
		    (above (beside/space first second)
			   (walk rest))]))
	       (let walk ((item-docs item-docs))
		 (match item-docs
		   ['()
		    (empty-doc)]
		   [(list last)
		    last]
		   [(list penultimate ultimate)
		    (above penultimate ultimate)]
		   [(list* first second rest)
		    (above* first
			    second
			    ";;"
			    (walk rest))])))
	      ")")]
    [_ (generic-sexp->doc 0 form)]))

;;---------------------------------------------------------------------------

(define current-racket-format-map
  (make-parameter
   (hash 'let let-sexp->doc
	 'let* let-sexp->doc
	 'letrec let-sexp->doc
	 'match case-like-sexp->doc
	 'cond cond-like-sexp->doc
	 'quote (generic-quoted-sexp->doc "'")
	 'quasiquote (generic-quoted-sexp->doc "`")
	 'unquote (generic-quoted-sexp->doc ",")
	 'unquote-splicing (generic-quoted-sexp->doc ",@")
	 'syntax (generic-quoted-sexp->doc "#'")
	 'quasisyntax (generic-quoted-sexp->doc "#`")
	 'unsyntax (generic-quoted-sexp->doc "#,")
	 'unsyntax-splicing (generic-quoted-sexp->doc "#,@")
	 'hash hash-like-sexp->doc
	 'lambda 1
	 'define 1
	 )))

;; Indents exclude the width of the opening paren.
(define current-big-indent (make-parameter 3))
(define current-normal-indent (make-parameter 1))

;;---------------------------------------------------------------------------

(define (sexp->doc form)
  (match form
    [(cons (? symbol? head) rest)
     (define formatter (hash-ref (current-racket-format-map) head #f))
     (if (procedure? formatter)
	 (formatter form)
	 (generic-sexp->doc formatter form))]
    [(? vector?)
     (beside "#" (sexp->doc (vector->list form)))]
    [_
     (with-output-to-string (lambda () (write form)))]))

(define (sexp->string form)
  (doc->string (sexp->doc form)))
