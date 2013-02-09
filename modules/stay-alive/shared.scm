;; Updated!
(define-module (stay-alive shared)
  #:use-module (shelf shelf)
  #:use-module (stay-alive ncurses-interface)
  #:use-module (stay-alive lang)
  #:use-module (srfi srfi-1)
  #:export (wrap-add-item
	    wrap-remove-item
	    dm
	    d
	    wrap-location))

(define wrap-remove-item 
  (method (item #:optional level agent) 
    (if (and level agent ($ item 'wielded? #:def #f)) 
	($ item 'unwield level agent))
    (if (and level agent ($ item 'worn? #:def #f)) 
	($ item 'take-off level agent))
    (if agent 
	(message-player 
	 level
	 ($ agent 'location) 
	 (sentence-agent-verb-item agent 'to-drop item)))
    (set! ($ item 'container) #f)
    (set! ($ this 'items) (delq item ($ this 'items)))))

(define wrap-location (method () (list ($ this 'row) ($ this 'col))))
(define (d count sides)
  (fold (lambda (side total) (+ total (+ 1 (random sides)))) 0 (iota count)))

(define-syntax dm
  (syntax-rules ()
    ((_ rolls sides) (method () (d rolls sides)))))
