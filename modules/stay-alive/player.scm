;; Updated!
(define-module (stay-alive player)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (stay-alive util)
  #:use-module (ice-9 receive)
  #:use-module (shelf shelf)
  #:use-module (shelf shelf-util)
  #:use-module (stay-alive ncurses-interface)
  #:use-module (stay-alive shared)
  #:use-module (stay-alive lang)
  #:use-module (stay-alive square)
  #:use-module (stay-alive agent)
  #:use-module (stay-alive game)
  #:export (Player Memory))

(define (player-examine-level agent level)
  ($ level 
     'describe 
     agent
     (player-select-square level ($ agent 'location) "select square")))

(define process-player-command 
  (lambda (level player command)
    (let* ((saved-command ($ player 'saved-command #:def #f))
           (count ($ player 'count #:def #f))
           (can-repeat-command? 
	    (lambda (command)
	      (case (if (list? command) (car command) command)
		((down up left right drop use up-left up-right down-left down-right) #t)
		((update-count terminal inventory pick-up quit reset-count) #f))))
           (result
             (if (list? command) 
               (case (car command)
                 ((down up left right up-left up-right down-left down-right) 
		  (if (eq? (cadr command) 'long) 
		      (move-agent-direction-long level player (car command))))
                 ((update-count) 
		  (let* ((count ($ player 'count #:def #f))
			 (new-count 
			  (string->number 
			   (if (not count) (cadr command)
			       (string-append (number->string count) (cadr command))))))
		    (message (format #f "repeat count: ~a" new-count))
		    (set! ($ player 'count) new-count) #f)))
               (case command
                 ((down up left right up-left up-right down-left down-right) 
		  (or
		   (move-agent-direction level player command)
		   (and-let* ((melee-target 
			       ($ level 'agent-at 
				  (make-new-location ($ player 'location) command))))
		     ($ player 'melee-attack melee-target level))))
                 ((terminal) (terminal the-game-module))
                 ((reset-count) 
		  (if count (begin (message "repeat count reset") 
				   (set! ($ player 'count) #f))) #f)
                 ((unknown) (message "I don't know what that means") #f)
		 ((follow-up-command) ($ player 'follow-up-command #:def #f))
                 ((wait) #t)		
		 ((close-door) 
		  (let ((door-direction (player-select-direction)))
		    (if (not (eq? door-direction 'none))
			(let ((door-location 
			       (make-new-location ($ player 'location) door-direction)))
			 (if ($ ($ level (list 'squares door-location))
				'open-door? #:def #f)
			     (if ($ level 'contains-agent? door-location)
				 (begin 
				   (message 
				    (format 
				     #f "~a is in the way!" 
				     (capitalize 
				      ($ ($ level 'agent-at door-location)
					 'describe)))) 
				   #f)
				 (begin
				   (message "You shut the door.")
				   ($ level 'set-square! 
				      (car door-location) 
				      (cadr door-location) 
				      (instance ClosedDoor))
				   ($ level 'remember 
				      (car door-location) (cadr door-location))
				   #t))
			     (begin
			       (message "There's no door there!")
			       #f)))
			(begin (message "never mind") #f))))
                 ((save) (set! ($ player 'status) 'saving) #t)
                 ((throw) ($ player 'throw level))
                 ((look) (player-examine-level player level) #f)
                 ((down-stairs) ($ ($ level `(squares (,(player 'row) ,(player 'col))))
				   'go-down player level))
                 ((up-stairs) ($ ($ level `(squares (,(player 'row) ,(player 'col)))) 
				 'go-up player level))
                 ((pick-up) ($ player 'pick-up level))
                 ((drop) ($ player 'drop level))
                 ((use) ($ player 'use-something level))
                 ((inventory) 
		  (display-items ($ player 'items) "inventory:" "inventory empty" player-confirm-callback))
                 ((quit) (set! ($ player 'status) 'quitting) #t)))))
      (if (and count (can-repeat-command? command))
        (if saved-command
          (if (= count 1)
            (begin (set! ($ player 'count) #f)
                   (set! ($ player 'saved-command) #f))
            (set! ($ player 'count) (- count 1)))
          (set! ($ player 'saved-command) command)))
      result)))

(define-object Memory 
  (items #f)
  (description #f)
  (square #f))

(define-object Player
  (initialize! 
   (method (#:optional inventory) 
     ((super this) 'initialize!)
     (for-each (lambda (item) ($ this 'add-item! item)) inventory)))
  (player? #t)
  (describe "you")
  (describe-definite "you")
  (describe-possessive "your")
  (name "You")
  (can-open-doors? #t)
  (throw 
   (method (level)
     (let ((item (player-select-item ($ this 'items) "throw what?" "nothing to throw"))
	   (item-in-flight 
	    (lambda (item location finish)
	      (set! ($ item 'row) (car location))
	      (set! ($ item 'col) (cadr location))
	      ($ level 'prepare-for! ($ level 'get-player))
	      (refresh-level-view level this)
	      finish)))
       (if item
	   (and-let* 
	       ((target (player-select-square level ($ this 'location) "choose target")))
	     ($ this 'remove-item! item)
	     ($ level 'add-item! item)
	     (clear-message)
	     (message (format #f "you throw ~a" ($ item 'describe-short)))
	     (line-for-each 
	      ($ this 'location) 
	      target 
	      (lambda (current previous distance) 
		(let ((agent ($ level 'agent-at current)))
		  (cond 
		   ((and agent ($ item 'hit-agent level this agent distance)) 
		    (item-in-flight item current #t))
		   (($ ($ level `(squares (,@current))) 'stops-projectiles?) 
		    (item-in-flight item previous #t))
		   ((equal? current target) (item-in-flight item current #t))
		   (else (item-in-flight item current #f)))))))))))
  (choose-weapon
   (method () 
     (let ((weapon-list 
	    (select-from-list 
	     (filter (pred-and 
		      (applicator 'weapon? #:def #f) 
		      (applicator 'wielded? #:def #f)) 
		     ($ this 'items)) 
	     (applicator 'describe-short) 
	     "Attack with what?" 
	     "" #:def (lambda () 
			(select-from-list ($ ($ this 'body) 'melee-parts)
					  (applicator 'describe)
					  "Attack with what?"
					  "You have nothing to attack with!")))))
       (if weapon-list (car weapon-list) #f))))
  (base-hp (method () (+ 10 (d 1 4))))
  (rarity 'never)
  (gender 'male)
  (describe-long 
   (method ()
     (string-append "a "
		    (if (eq? ($ this 'gender) 'male)
			"dapper little fellow"
			"charming little lady")
		    " who seems to have fallen in with the wrong dungeon")))
  (use-something 
   (method (level)
     (let ((item (player-select-item ($ this 'items) "use what?" "nothing to use")))
       (if item ($ item 'use level this)))))
  (square-interact 
   (method (level location)
     (let ((items ($ level 'items-at location)))
       (if (= (length items) 1)
	   (message ($ (car items) 'describe-short))
	   (if (> (length items) 1) (message "Multiple items here"))))))
  (drop 
   (method (level)
     (let ((item (player-select-item ($ this 'items) "drop what?" "nothing to drop")))
       (if (and item ($ level 'add-item! item))
	   (begin
	     (set! ($ item 'location) ($ this 'location))
	     ($ this 'remove-item! item level this)
	     #t)
	   #f))))
  (pick-up 
   (method (level)
     (let ((item 
	    (player-select-item 
	     (filter 
	      (lambda (item) 
		(receive (row col)
		    (location-values ($ item 'location)) 
		  (and (= row ($ this 'row)) (= col ($ this 'col))))) ($ level 'items)) 
	     "pick up what?"
	     "no items here")))
       (if (and item ($ this 'add-item! item))
	   (begin
	     (message (format #f "picked up ~a (~a)" 
			      ($ item 'describe-short) 
			      ($ item 'letter)))
	     ($ level 'remove-item! item)
	     #t)
	   #f))))
  (take-turn (method (paths-to-player player level)
	       (if ($ this 'interrupted?)
		   (begin
		     (set! ($ this 'count) #f)
		     (set! ($ this 'saved-command) #f)))
	       (let ((command (or ($ this 'saved-command #:def #f) (get-command))))
		 (clear-message)
		 (process-player-command level this command))))
  (symbol 'at))

(extend Humanoid (list Player))
