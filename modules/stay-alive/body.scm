(define-module (stay-alive body)
  #:use-module (shelf shelf)
  #:use-module (shelf shelf-util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-69)
  #:use-module (stay-alive lang)
  #:use-module (stay-alive ncurses-interface)
  #:use-module (stay-alive util)
  #:use-module (ice-9 format)
  #:export (select-part-from-list))

(define* (create-body parts #:optional inherited-side)
  (fold-right 
   append '()
   (map (lambda (part-spec)
	  (let ((main-part-spec (car part-spec))
		(make-child-parts 
		 (lambda (side) 
		   (if (null? (cadr part-spec)) 
		       #f 
		       (list (create-body (cadr part-spec) (or side inherited-side)) side)))))
	    (if (list? main-part-spec)
		(cond
		  ((list? (cadr main-part-spec))
		  (map (lambda (side) (instance (car main-part-spec)
						#:args (make-child-parts side))) (cadr main-part-spec)))
		 ((number? (cadr main-part-spec))
		  (map (lambda (n) 
			 (instance (car main-part-spec) #:args (make-child-parts inherited-side))) 
		       (iota (cadr main-part-spec))))
		 ((symbol? (cadr main-part-spec))
		  (list (instance (car main-part-spec) 
				  (m (,(cadr main-part-spec) #t)) #:args (make-child-parts inherited-side))))
		 (else (display (cadr main-part-spec)) (newline)))
		(list (instance main-part-spec #:args (make-child-parts inherited-side))))))
	parts)))

(define-objects-public Body  
  ((find (method (pred) (filter pred (this 'all-parts))))
   (add-part (method (new-part to-add-to)
	       (and-let* ((matching-to-add-to (this 'find (pred-proc-eq to-add-to (applicator 'name)))))
		 (set! (matching-to-add-to 'parts) (cons new-part (matching-to-add-to 'parts))))))
   (all-parts (method ()
		(fold-right append '() (map (applicator 'all-parts) (this 'parts)))))
   (melee-parts (method ()
		  (fold-right append '() (map (applicator 'melee-parts) (this 'parts)))))
   (choose-by-weight (method ()
		       (let* ((weights 
			       (map 
				(lambda (part) (list part (hashq-ref body-part-size-weights (part 'size)))) 
				(this 'all-parts)))
			      (sum (fold + 0 (map (lambda (weight) (cadr weight)) weights)))
			      (choice (random sum)))
			 (let select ((accum 0) (weights weights))
			   (if (> (+ accum (cadar weights)) choice) (caar weights)
			       (select (+ accum (cadar weights)) (cdr weights)))))))
   (can-spin-webs? 
    (method ()
      (not (null?
	    (this 'find 
		  (pred-and 
		   (pred-proc-eq #f (applicator 'disabled? #:def #f)) 
		   (pred-proc-eq #t (applicator 'spins-webs? #:def #f))))))))
   (blind? 
    (method ()	    
      (null?
       (this 'find
	     (pred-and
	      (pred-proc-eq #f (applicator 'disabled? #:def #f))
	      (pred-proc-eq #t (applicator 'can-see? #:def #f))))))))
  ((UnknownBody  
    ((initialize!
      (method ()
	(set! (this 'parts)
	      (create-body `((,Abdomen ()))))))))
   (ArachnidBody
    ((initialize!
      (method ()
	(set! (this 'parts)
	      (create-body `((,Cephalothorax (((,Eye 8) ())					 
					      ((,Pedipalp 2) ())
					      ((,Leg 4) ())))
			     (,Abdomen (((,Leg 4) ()))))))))))
   (SlugBody  
    ((initialize!
      (method ()
	(set! (this 'parts)
	      (create-body `((,Abdomen
			      (((,Eye 4) ()))))))))))
   (HumanoidBody  
    ((initialize! 
      (method ()
	(set! (this 'parts) 
	      (create-body
	       `((,Abdomen 
		  ((,Neck
		    ((,Head (((,Ear ("left" "right")) ())
			     ((,Eye ("left" "right")) ())
			     (,Nose ())
			     (,Mouth (((,Tooth multiple?) ())))))))
		   ((,Arm ("left" "right"))
		    ((,Hand (((,Finger 5) ())))))
		   ((,Leg ("left" "right"))
		    ((,Foot (((,Toe 5) ()))))))))))))))))

(define body-part-size-weights
  (h
   (none 0)
   (smallest 8)
   (smaller 10)
   (small 50)
   (large 100)
   (larger 200)
   (largest 400)))

(define (select-part-from-list eligible-parts prep quantity)
  (select-from-list eligible-parts
		    (lambda (part) (format #f "~6,,,@a ~a" (part 'side #:def "") (part 'name)))
		    (format #f "~a what?" (capitalize prep))
		    "You don't have any space for that!"
		    quantity))

(define-objects-public BodyPart  
  ((initialize! (method (#:optional parts side)
		  (if parts
		      (set! (this 'parts) (if (list? parts) parts (list parts))))
		  (if side
		      (set! (this 'side) side))))		
   (describe (method ()
	       (format #f "~a~a"
		       (if (this 'side #:def #f) (format #f "~a " (this 'side)) "")
		       (this 'name))))
   (dismember (method (level agent) 
		(message-player level (agent 'location) (format #f "~a ~a is dismembered!" 
								(agent 'describe-possessive) 
								(this 'describe)))
		(set! (this 'injured?) #t)
		(set! (this 'disabled?) #t)
		(set! (this 'dismembered?) #t)))
   (injure (method (level agent) 				
	     (message-player level (agent 'location) (format #f "~a ~a is injured!" 
							     (agent 'describe-possessive) 
							     (this 'describe)))
	     (set! (this 'injured?) #t)))
   (disable (method (level agent) 
	      (message-player level (agent 'location) (format #f "~a ~a is disabled!" 
							      (agent 'describe-possessive)
							      (this 'describe)))
	      (set! (this 'injured?) #t)
	      (set! (this 'disabled?) #t)
	      (map
	       (lambda (item) (item 'unwield level agent))
	       (filter (lambda (item) (find (pred-eq this) (item 'wielded-with #:def '()))) (agent 'items)))))
   (heal (method (level agent) 
	   (set! (this 'disabled?) #f)
	   (set! (this 'dismembered?) #f)
	   (set! (this 'injured?) #f)))		
   (all-parts
    (method () (cons this (fold-right append '() (map (applicator 'all-parts) (this 'parts #:def '()))))))
   (melee-parts 
    (method () (fold-right append '() 
			   (map (lambda (part) (matching-instances (applicator 'can-melee? #:def #f) 'parts part)) 
				(this 'parts)))))
   (can-melee? #f))
((Arm
  ((name 'arm)
   (size 'large)))
 (Neck
  ((name 'neck)
   (size 'small)))
 (Hand
  ((name 'hand)
   (can-melee? #t)
   (size 'small)))
 (Finger
  ((name 'finger)
   (size 'smaller)))
 (Leg
  ((name 'leg)
   (size 'larger)))
 (Foot
  ((name 'foot)
   (can-melee? #t)
   (size 'small)))
 (Toe
  ((name 'toe)
   (size 'smaller)))
 (Head
  ((name 'head)
   (size 'large)))
 (Eye  
  ((can-see? #t)
   (disable (method (level agent) 
	      (super 'disable level agent)				  
	      (if ((agent 'body) 'blind?)
		  (message-player level (agent 'location) 
				  (format #f "~a ~a blind!"
					  (capitalize (agent 'describe-definite)) 
					  (conjugate-verb 'to-be (agent 'player?)))))))
   (heal (method (level agent)
	   (super 'heal level agent)					 
	   (message-player level (agent 'location)
			   (format #f "~a can see!"
				   (capitalize (agent 'describe-definite))))))
   (size 'smallest)
   (name 'eye)))
 (Ear  
  ((size 'smallest)
   (name 'ear)))
 (Nose  
  ((size 'smallest)
   (name 'nose)))
 (Mouth  
  ((size 'smallest)
   (name 'mouth)))
 (Tooth
  ((name 'tooth)
   (size 'none)
   (can-melee? #t)))
 (Abdomen
  ((name 'abdomen)
   (size 'largest)))
 (Cephalothorax
  ((name 'cephalothorax)
   (size 'largest)))
 (Pedipalp
  ((name 'pedipalps)
   (size 'smallest)))
 (Spinneret
  ((spins-webs? #t)
   (name 'spinneret)
   (size 'smallest)))
 (Stinger
  ((name 'stinger)
   (can-melee? #t)
   (poisonous? #t)
   (size 'large)))))