;; Updated!
(define-module (stay-alive lang)
  #:use-module (shelf shelf)
  #:export (indefinite-article-for
	    capitalize
	    conjugate-passive-verb
	    sentence-agent-verb-item	    
	    sentence-agent-verb-agent
	    sentence-agent-verb
	    sentence-agent-verb-passive
	    sentence-agent-verb-agent-possessive
	    sentence-possessive-item-verb-passive
	    conjugate-verb))

(define (capitalize str)
  (string-append (string-capitalize (string-take str 1)) (string-drop str 1)))

(define (indefinite-article-for name)
  (case (string-ref (if (list? name) (car name) name) 0) 
    ((#\a #\e #\i #\o #\u) "an") 
    (else "a")))

(define-syntax double-case
  (syntax-rules ()
    ((_ p1 p2 ((v) r1 r2) ... (else def1 def2))
     (case p1
       ((v) (if p2 r1 r2)) ...
       (else (if p2 def1 def2))))
    ((_ p1 p2 ((v) r1 r2) ...)
     (case p1
       ((v) (if p2 r1 r2)) ...))))

(define (sentence text)
  (format #f "~a." (capitalize text)))

(define (agent-name agent)
  (if (string? agent) agent ($ agent 'describe-definite)))

(define (possessive agent)
  (cond
   ((string? agent) agent)
   ((agent 'player?) "your") 
   (else (format #f "~a's" ($ agent 'describe-definite)))))

(define (conjugate-passive-verb verb player?)
  (let* ((infinitive-string (cadr (string-split (symbol->string verb) #\-))) 
	 (verb-string (string-append infinitive-string
				     (case (string-ref (string-reverse infinitive-string) 0) 
				       ((#\e) "d")
				       (else "ed")))))
    (double-case verb player?
		 (else (string-append "are " verb-string) (string-append "is " verb-string)))))

(define (sentence-agent-verb-agent-possessive actor verb recipient possession)
  (sentence
   (format #f "~a ~a ~a ~a"
	   (agent-name actor)
	   (conjugate-verb verb ($ actor 'player?))
	   (possessive recipient)
	   possession)))

(define (sentence-possessive-item-verb-passive agent possession verb)
  (sentence
   (format #f "~a ~a ~a"
	   (possessive agent)
	   possession
	   (conjugate-passive-verb verb #f))))

(define (sentence-agent-verb-item agent verb item)
  (format #f "~a ~a ~a." 
	  (capitalize ($ agent 'describe-definite))
	  (conjugate-verb verb ($ agent 'player?))
	  ($ item 'describe-definite)))

(define (sentence-agent-verb-agent actor verb recipient)
  (format #f "~a ~a ~a."
	  (if (string? actor) actor (capitalize ($ actor 'describe-definite)))
	  (conjugate-verb verb ($ actor 'player?))
	  (if (string? recipient) recipient ($ recipient 'describe-definite))))

(define (sentence-agent-verb actor verb)
  (format #f "~a ~a."
	  (capitalize ($ actor 'describe-definite))
	  (conjugate-verb verb ($ actor 'player?))))

(define (sentence-agent-verb-passive actor verb)
  (format #f "~a ~a."
	  (capitalize ($ actor 'describe-definite))
	  (conjugate-passive-verb verb ($ actor 'player?))))

(define (conjugate-verb  verb player?)
  (let ((verb-string (cadr (string-split (symbol->string verb) #\-))))
    (double-case verb player?
		 ((to-be) "are" "is")
		 ((to-put-on) "put on" "puts on")
		 ((to-nibble-on) "nibble on" "nibbles on")
		 ((to-take-off) "take off" "takes off")
		 (else verb-string (case (string-ref (string-reverse verb-string) 0)
				     ((#\h #\x) ((string-append verb-string "es")))
				     (else (string-append verb-string "s")))))))
