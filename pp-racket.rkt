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
	 generic-quoted-sexp->doc
	 hash-like-sexp->doc
	 )

;;---------------------------------------------------------------------------

(define (generic-sexp->doc special-argument-count form #:parens [parens 'round])
  (define-values (LP RP) (match parens
			   ['round (values "(" ")")]
			   ['square (values "[" "]")]
			   ['curly (values "{" "}")]
			   [(cons l r) (values l r)]))

  (match-define (cons head args) form)
  (define head-doc (sexp->doc head))
  (define arg-docs (map sexp->doc args))

  (define (layout-variations distance arg-docs)
    (choice (beside/space head-doc (apply above* arg-docs))
	    (above head-doc (indent distance (apply above* arg-docs)))))

  (beside* LP
	   (choice (apply beside*/space (cons head-doc arg-docs))
		   (cond
		    [(not special-argument-count)
		     (layout-variations 0 arg-docs)]
		    [(zero? special-argument-count)
		     (layout-variations (current-normal-indent) arg-docs)]
		    [(positive? special-argument-count)
		     (define special-arg-docs (take-at-most arg-docs special-argument-count))
		     (define ordinary-arg-docs (drop-at-most arg-docs special-argument-count))
		     (above (layout-variations (current-big-indent) special-arg-docs)
			    (indent (current-normal-indent) (apply above* ordinary-arg-docs)))]))
	   RP))

(define (let-sexp->doc form)
  (match form
    [`(let ,(? symbol?) ,_ ...) (generic-sexp->doc 2 form)]
    [_ (generic-sexp->doc 1 form)]))

(define (cond-like-sexp->doc form)
  (match form
    [`(,head ,value [,test ,body ...] ...)
     (define head-doc (sexp->doc head))
     (beside* "("
	      (above
	       (choice (beside/space head-doc (sexp->doc value))
		       (above head-doc
			      (indent (current-big-indent) (sexp->doc value))))
	       (indent (current-normal-indent)
		       (apply above*
			      (map (lambda (test body-forms)
				     (define vertical
				       (beside* "["
						(above (sexp->doc test)
						       (apply above* (map sexp->doc body-forms)))
						"]"))
				     (match body-forms
				       [(list body-form)
					(choice (beside* "["
							 (sexp->doc test)
							 " "
							 (sexp->doc body-form)
							 "]")
						vertical)]
				       [_ vertical]))
				   test
				   body))))
	      ")")]
    [_ (generic-sexp->doc 1 form)]))

(define ((generic-quoted-sexp->doc prefix) form)
  (match form
    [(list _ body)
     (beside prefix (sexp->doc body))]
    [_ (generic-sexp->doc 1 form)]))

(define (hash-like-sexp->doc form)
  (match form
    [(cons head items)
     (beside* "("
	      (sexp->doc head)
	      (choice
	       (let walk ((items items))
		 (match items
		   ['()
		    (empty-doc)]
		   [(list last)
		    (sexp->doc last)]
		   [(list* first second rest)
		    (above (beside/space (sexp->doc first) (sexp->doc second))
			   (walk rest))]))
	       (let walk ((items items))
		 (match items
		   ['()
		    (empty-doc)]
		   [(list last)
		    (sexp->doc last)]
		   [(list penultimate ultimate)
		    (above (sexp->doc penultimate) (sexp->doc ultimate))]
		   [(list* first second rest)
		    (above* (sexp->doc first)
			    (sexp->doc second)
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
	 'match cond-like-sexp->doc
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

(require racket/trace)
(trace cond-like-sexp->doc)
