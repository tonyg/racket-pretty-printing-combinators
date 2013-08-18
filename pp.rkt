#lang racket/base
;; Pretty-printing combinators, loosely following Azero and Swierstra,
;; but using ordered choice instead of unordered choice. The change
;; lets us find an OK-ish solution in reasonable time, but of course
;; means we're not guaranteed to find an optimal solution.
;;
;; See http://www.cs.uu.nl/wiki/HUT/PrettyPrintingCombinators.

(require racket/match)
(require racket/stream)
(require racket/promise)
(require (only-in racket/string string-join))
(require (only-in racket/function curry))
(require "stream-utils.rkt")

(provide ;; Document representation
         (struct-out empty-doc)
	 (struct-out indent)
	 (struct-out above)
	 (struct-out beside)
	 (except-out (struct-out choice) choice)
	 (rename-out [build-choice choice])

	 ;; Page control
	 current-page-width

	 ;; Derived constructors
	 above*
	 beside/space
	 beside*
	 beside*/space
	 seq-docs

	 ;; Primary rendering interface
	 doc->string

	 ;; Access to partially-formatted structures and low-level rendering tools
	 (struct-out element)
	 (struct-out text-structure)
	 doc->formats
	 doc->format
	 format->lines
	 format->string
	 )

;;---------------------------------------------------------------------------

;; A PPDoc is a layoutable document, one of
;;  -- (empty-doc), the empty document
;;  -- a String, an atomic piece of text
;;  -- an (indent Natural PPDoc), an indented document
;;  -- an (above PPDoc PPDoc), the first placed "above" the second
;;  -- a (beside PPDoc PPDoc), the first placed "beside" the second
;;  -- a (choice (Streamof PPDoc)), rendering as whichever "best" fits on the page
(struct empty-doc () #:prefab)
(struct indent (distance doc) #:prefab)
(struct above (top bottom) #:prefab)
(struct beside (left right) #:prefab)
(struct choice (choices) #:prefab)

;; A Format is an (element Natural Natural Natural TextStructure)
;; Contains a text structure and its measurements.
(struct element (height last-line-width total-width text-structure) #:prefab)

;; A TextStructure represents a laid-out piece of text.
;; It is a (text-structure (Listof (String -> String)) (String -> String)).
(struct text-structure (body last-line) #:prefab)

;;---------------------------------------------------------------------------

;; Width of the page we are laying out into, in character positions.
(define current-page-width (make-parameter 80))

;;---------------------------------------------------------------------------
;; Derived PPDoc constructors.

(define (above* . xs)
  (foldr above (empty-doc) xs))

(define (beside/space l r)
  (cond
   [(empty-doc? l) r]
   [(empty-doc? r) l]
   [else (beside l (beside " " r))]))

(define (beside* . xs)
  (foldr beside (empty-doc) xs))

(define (beside*/space . xs)
  (foldr beside/space (empty-doc) xs))

(define-syntax-rule (build-choice exp ...)
  (choice (stream exp ...)))

(define (seq-docs hlparen hsep hrparen vlparen vsep vrparen ds)
  (define (walk comb sep ds)
    (match ds
      ['() (empty-doc)]
      [(list d) d]
      [(cons d rest) (comb (beside d sep) (walk comb sep rest))]))
  (build-choice (beside* hlparen (walk beside hsep ds) hrparen)
		(beside* vlparen (walk above vsep ds) vrparen)))

;;---------------------------------------------------------------------------

;; Format shape equality.
(define (format-shape=? a b)
  (match-define (element ah alw atw _) a)
  (match-define (element bh blw btw _) b)
  (and (= ah bh) (= atw btw) (= alw blw)))

;; Format shapes are orderable.
(define (format-shape<? a b)
  (match-define (element ah _ atw _) a)
  (match-define (element bh _ btw _) b)
  (define a-overflow? (> atw (current-page-width)))
  (define b-overflow? (> btw (current-page-width)))
  (cond
   [(and a-overflow? (not b-overflow?)) #f]
   [(and b-overflow? (not a-overflow?)) #t]
   [else (or (< ah bh) (and (= ah bh) (< atw btw)))]))

(define (empty-format? e)
  (match e
    [(element 0 0 0 _) #t]
    [_ #f]))

;;---------------------------------------------------------------------------
;; Functions producing various TextStructure instances.

(define (empty-text-structure)
  (text-structure '() values))

(define (string-text-structure s)
  (text-structure '() (curry string-append s)))

(define (indent-text-structure distance t)
  (match-define (text-structure tb tll) t)
  (define prefix (make-string distance #\space))
  (define (prefix-fn s) (string-append prefix s))
  (text-structure (map (curry compose prefix-fn) tb)
		  (compose prefix-fn tll)))

(define (above-text-structure t b)
  (match-define (text-structure tb tll) t)
  (match-define (text-structure bb bll) b)
  (text-structure (append tb (list tll) bb) bll))

(define (beside-text-structure left-last-line-width l r)
  (match-define (text-structure lb lll) l)
  (match-define (text-structure rb rll) r)
  (match rb
    ['()
     (text-structure lb (compose lll rll))]
    [(cons rb1 rbN)
     (define prefix (make-string left-last-line-width #\space))
     (define (prefix-fn s) (string-append prefix s))
     (text-structure (append lb (cons (compose lll rb1)
				      (map (curry compose prefix-fn) rbN)))
		     (compose prefix-fn rll))]))

;;---------------------------------------------------------------------------
;; Functions producing various Format instances and lists of
;; alternative Format instances.

(define (empty-format)
  (element 0 0 0 (delay (empty-text-structure))))

(define (indent-format distance e)
  (match e
    [(element 0 0 0 _) e]
    [(element dh dlw dtw dts)
     (element dh
              (+ distance dlw)
	      (+ distance dtw)
	      (delay (indent-text-structure distance (force dts))))]))

(define (stream-filter-keeping-least-bad ? xs)
  (define filtered-xs (stream-filter ? xs))
  (if (stream-empty? filtered-xs)
      (if (stream-empty? xs)
  	  '()
  	  (list (stream-first xs))) ;; TODO: is this the least ugly/bad element to pick?
      filtered-xs))

(define ((fits? indent-distance) e)
  (<= (element-total-width e) (- (current-page-width) indent-distance)))

(define (above-format t b)
  (cond
   [(empty-format? t) b]
   [(empty-format? b) t]
   [else (match-define (element th tlw ttw tts) t)
	 (match-define (element bh blw btw bts) b)
	 (element (+ th bh)
	          blw
		  (max ttw btw)
		  (delay (above-text-structure (force tts) (force bts))))]))

(define (beside-format l r)
  (cond
   [(empty-format? l) r]
   [(empty-format? r) l]
   [else (match-define (element lh llw ltw lts) l)
	 (match-define (element rh rlw rtw rts) r)
	 (element (+ lh rh -1)
	          (+ llw rlw)
		  (max ltw (+ llw rtw))
		  (delay (beside-text-structure llw (force lts) (force rts))))]))

;;---------------------------------------------------------------------------
;; Formatting and rendering a document.

(define (doc->formats d)
  (define (walk d)
    (match d
      [(empty-doc)
       (list (empty-format))]
      [(? string? s)
       (define l (string-length s))
       ;; Used to check width here, returning '() if we overflowed the
       ;; page; now we rely on the overflow-reordering trick instead, so
       ;; that we get to keep some candidate layout even in extreme
       ;; circumstances.
       (list (element 1 l l (delay (string-text-structure s))))]
      [(indent distance doc)
       (stream-map (curry indent-format distance)
		   (stream-filter-keeping-least-bad (curry fits? distance) (walk doc)))]
      [(above t b)
       (define ts (walk t))
       (define bs (walk b))
       (rational-stream-map above-format ts bs)]
      [(beside l r)
       (define ls (walk l))
       (define rs (walk r))
       (define (fits-beside? l r)
	 (<= (max (element-total-width l)
		  (+ (element-last-line-width l) (element-total-width r)))
	     (current-page-width)))
       (append-streams (stream-map
			(lambda (l)
			  (stream-map (curry beside-format l)
				      (stream-filter-keeping-least-bad (curry fits-beside? l)
								       rs)))
			ls))]
      [(choice ds)
       (interleave-streams (stream-map walk ds))]))

  (define raw (walk d))
  (define filtered (stream-filter (fits? 0) raw))
  (if (stream-empty? filtered)
      raw
      filtered))

(define (doc->format d)
  (define fs (doc->formats d))
  (and (not (stream-empty? fs))
       (stream-first fs)))

(define (format->lines e)
  (match-define (element _ _ _ ts) e)
  (match-define (text-structure b ll) (force ts))
  (append (map (lambda (f) (f "")) b)
	  (list (ll ""))))

(define (format->string e [newline "\n"])
  (string-join (format->lines e) newline))

(define (doc->string d)
  (define f (doc->format d))
  (and f (format->string f)))

;;---------------------------------------------------------------------------
;; TODO

;; (Listof PPDoc) -> (Listof Format)
;; TODO: is this sensible??
;; (define (horizontal-or-vertical docs)
;;   (define alts (map doc->formats docs))
;;   (define vertical-formats (foldr above-formats (empty-formats) alts))
;;   (define (height-one? e) (= (element-height e) 1))
;;   (if (andmap height-one? alts)
;;       (choice-formats (foldr beside-formats (empty-formats) alts) vertical-formats)
;;       vertical-formats))
