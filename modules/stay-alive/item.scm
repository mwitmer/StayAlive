;; Updated!
(define-module (stay-alive item)
  #:use-module (shelf shelf)
  #:use-module (shelf shelf-util)
  #:use-module (stay-alive ncurses-interface)
  #:use-module (stay-alive weight)
  #:use-module (stay-alive body)
  #:use-module (stay-alive timer)
  #:use-module (stay-alive lang)
  #:use-module (stay-alive util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (stay-alive shared)
  #:export (Item Floodlight Torch))

(define-objects Item
  ((initialize! (method ()
		  (set! ($ this 'timers) '())))
   (quantity 1)
   (lit? #f)
   (weapon? #f)
   (item? #f)
   (hit-agent 
    (method (level source target distance)
      (message-player level 
		      ($ target 'location) 
		      (format #f "~a ~a ~a" 
			      ($ this 'describe-definite) 
			      (conjugate-verb 'to-hit #f) 
			      ($ target 'describe-definite)))))
   (describe-inventory 
    (method ()
      (format #f "~a - ~a" ($ this 'letter) ($ this 'describe-short))))
   (describe-definite
    (method ()
      (let ((quantity ($ this 'quantity))
	    (name ($ this 'name)))
	(format #f "the ~a" (if (> quantity 1) 
				(if (list? name) (cadr name) (string-append name "s"))
				(if (list? name) (car name) name))))))
   (describe-short 
    (method ()
      (let* ((quantity ($ this 'quantity))
	     (name ($ this 'name))
	     (base (if (= quantity 1)
		       (format #f "~a ~a" (indefinite-article-for name) (if (list? name) (car name) name))
		       (format #f "~a ~a" quantity (if (list? name) (cadr name) (string-append name "s"))))))
	(fold (lambda (prop prev) (if ($ this (car prop) #:def #f) (format #f "~a (~a)" prev (cdr prop)) prev)) 
	      base 
	      '((wielded? . "wielded")
		(worn? . "worn")
		(lit? . "lit"))))))
   (wield-requires (h (quantity 1)
		      (pred (method (part) (eq? ($ part 'name) 'hand)))))
   (put-on 
    (method (level agent)				
      (and-let* 
	  ((eligible-parts
	    (let ((wear-matches 
		   ($ ($ agent 'body) 'find 
		      (pred-and (object-get this '(wear-requires pred))
				(pred-not (applicator 'wearing? #:def #f))))))
	      (if ($ this '(wear-requires also-wield?) #:def #f)
		  (filter 
		   (lambda (part)
		     (not (fold 
			   (lambda (a b) (or a b)) #f 
			   (map (lambda (part) ($ part 'wielding? #:def #f)) 
				(filter 
				 (lambda (part)
				   (equal? ($ part 'name) 
					   ($ this '(wear-requires also-wield?)))) 
				 ($ part 'all-parts)))))) wear-matches)
		  wear-matches)))
	   (parts 
	    (select-part-from-list eligible-parts "on" (object-get this '(wear-requires quantity) #:def 1))))
	(if parts
	    (begin
	      (for-each (lambda (part) (set! ($ part 'wearing?) #t)) parts)
	      (if ($ this '(wear-requires also-wield?) #:def #f)
		  (for-each 
		   (lambda (part)
		     (map
		      (obj-setter 'wielding? #t)
		      (filter (lambda (part) 
				(eq? ($ part 'name) 
				     ($ this '(wear-requires also-wield?))))             
			      ($ part 'all-parts)))) parts))
	      (set! ($ this 'worn?) #t)						      
	      (set! ($ this 'worn-on) (map object-reference parts))
	      (message-player level ($ agent 'location) (sentence-agent-verb-item agent 'to-put-on this))
	      #t)
	    #f))))
   (take-off 
    (method (level agent)
      (and-let* ((($ this 'worn? #:def #f))
		 (parts ($ this 'worn-on #:def #f)))
	(for-each (obj-setter 'wearing? #f) parts)
	(if ($ this '(wear-requires also-wield?) #:def #f)
	    (for-each 
	     (lambda (part)
	       (map
		(obj-setter 'wielding? #f)
		(filter (lambda (part) (eq? ($ part 'name) ($ this '(wear-requires also-wield?)))) 
			($ part 'all-parts)))) parts))
	(message-player level ($ agent 'location) (sentence-agent-verb-item agent 'to-take-off this))
	(set! ($ this 'worn-on) #f)
	(set! ($ this 'worn?) #f)
	#t)))
   (unwield 
    (method (level agent)
      (let ((parts ($ this 'wielded-with #:def #f)))
	(if parts (for-each (obj-setter 'wielding? #f) parts)))
      (if ($ this 'wielded? #:def #f) 
	  (begin
	    (message-player level ($ agent 'location) (sentence-agent-verb-item agent 'to-unwield this))
	    (set! ($ this 'wielded-with) #f)
	    (set! ($ this 'wielded?) #f)
	    #t)
	  #f)))
   (wield 
    (method (level agent)
      (and-let* 
	  ((eligible-parts 
	    ($ ($ agent 'body) 'find (pred-and (object-get this '(wield-requires pred)) 
					       (pred-not (applicator 'wielding? #:def #f)))))
	   (parts 
	    (select-part-from-list eligible-parts "with" (object-get this '(wield-requires quantity) #:def 1))))
	(if parts
	    (begin (for-each (lambda (part) (set! ($ part 'wielding?) #t)) parts)
		   (set! ($ this 'wielded?) #t)
		   (set! ($ this 'wielded-with) (map object-reference parts))
		   (message-player level ($ agent 'location) (sentence-agent-verb-item agent 'to-wield this))
		   #t)
	    #f)))))
  ((Lightsource 
    ((lightable? #t)
     (use (method (level agent) 	
	    (if ($ this 'lightable?)
		(begin (message-player
			level ($ agent 'location) 
			(sentence-agent-verb-item agent (if ($ this 'lit?) 'to-extinguish 'to-light) this))
		       (set! ($ this 'lit?) (not ($ this 'lit?)))
		       #t)
		(if (agent 'player?) 
		    (begin (refresh-level-view)
			   (message (format #f "~a is burnt out." 
					    (capitalize ($ this 'describe-definite))))) #f))))
     (symbol 'tool))
    ((Floodlight
      ((name "floodlight")
       (weight (pounds 10))
       (light-radius 80)))
     (Torch  
      ((initialize! 
	(method ()
	  (set! ($ this 'timers) 
		(list (instance LightSourceTimer #:args `(,this 2000))))))
       (name '("torch" "torches"))
       (weight (pounds 4))
       (light-radius 4)))))))
