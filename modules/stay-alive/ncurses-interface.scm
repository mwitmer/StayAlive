(define-module (stay-alive ncurses-interface)
  #:use-module (ncurses curses)
  #:use-module (stay-alive util)
  #:use-module (statprof)
  #:use-module ((srfi srfi-1) #:select ((map . srfi-map) fold fold-right find))
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 receive)
  #:use-module (stay-alive extensions)
  #:export (refresh-level-view
	    player-select-item
	    display-memory-items
	    display-items
	    get-command
	    display-refresh
	    terminal
	    player-confirm-callback
	    ask-yes-no
	    player-confirm
	    select-from-list
	    message
	    player-select-direction
	    message-player
	    init-display
	    clear-message
	    display-clear
	    player-select-square
	    display-clean-up))

(define stdscr #f)

(define (display-refresh) (refresh stdscr))
(define (display-clear) #f)

(define* (init-display #:optional new-stdscr)
  (set! stdscr (or new-stdscr (initscr)))
  (start-color!)
  (noecho!)
  (use-default-colors)
  (init-pair! 1 COLOR_WHITE -1)
  (init-pair! 2 COLOR_YELLOW -1)
  (init-pair! 3 COLOR_RED -1))

(define (ask-yes-no msg)
  (message (format #f "~a (y/n) " msg))
  (let get-answer ((answer (getch stdscr)))
    (case answer
      ((#\y) #t)
      ((#\n) #f)
      (else (get-answer (getch stdscr))))))

(define terminal 
  (lambda (environment)
    (while 
	#t
      (catch 
	#t
	(lambda () 
	  (addstr stdscr (format #f "~80,,a" "guile>") #:y 0 #:x 0)
	  (echo!)
	  (let* ((input (getnstr stdscr 80 #:y 0 #:x 7))
		 (result (eval-string input environment))
		 (pretty-result (format #f "~80,,s" result)))
	    (clear stdscr)
	    (refresh-level-view)
	    (addstr stdscr pretty-result #:y 1 #:x 0)))
	(lambda (key . parameters)
	  (noecho!)
	  (if (eq? key 'quit) 
	      (begin 
		(clear stdscr)
		(refresh-level-view)
		(break)))
	  (addstr stdscr (format #f "Error in input: ~s\n" key) #:y 1 #:x 0)
	  (format #f "~80,,s" parameters))))
    #f))

(define player-confirm-callback (lambda (y) (player-confirm y #:x 5)))

(define player-confirm 
  (lambda* (y #:key x) (addstr stdscr "-more-" #:x (if x x 0) #:y y) (getch stdscr) (clear-message)))

(define message-flow
  (let ((col 0))
    (lambda (text)
      (if (= 0 (string-length text))
	  (set! col 0)
	  (let render-string ((str text))
	    (call-with-values 
		(lambda ()
		  (search-string
		   str
		   (lambda (str idx)
		     (eq? (string-ref str idx) #\ ))
		   (lambda (str idx)
		     (values (string-take str idx) (string-drop str (+ 1 idx))))
		   (lambda (str)
		     (values str ""))))
	      (lambda (first-part last-part) 
		(addstr stdscr (format #f "~a " first-part) #:x col #:y 0)	    
		(refresh stdscr)
		(set! col (+ col 1 (string-length first-part)))
		(if (> (+ col (string-length first-part)) 73)
		    (begin (player-confirm 1) (set! col 0)))
		(if (not (= (string-length last-part) 0)) (render-string last-part)))))))))

(define message (lambda* (text #:key x y) 
			 (if (or x y)
			     (addstr stdscr (format #f "~a" text) #:x (if x x 0) #:y (if y y 0))
			     (message-flow text))))

(define message-player 
  (lambda* (level location text #:key x y)
    (let* ((player (level 'get-player))
	   (light-sources
	    (append (map (lambda (item) (cons (item 'location) (item 'light-radius))) 
			 (filter (lambda (item) (item 'lit?)) (level 'items)))
		    (fold (lambda (agent els) 
			    (append els (map (lambda (item) (cons (agent 'location) (item 'light-radius))) 
					     (filter (lambda (item) (item 'lit?)) (agent 'items))))) 
			  '() 
			  (level 'agents))))
	   (see-bits 
	    (see-level (level 'opacity) light-sources 80 (player 'location) (array-dimensions (level 'squares))))
	   (visibility 
	    (if (player 'blind?) 
		(make-bitvector 
		 (* (car (array-dimensions (level 'squares))) (cadr (array-dimensions (level 'squares)))) #f)
		(cdr see-bits))))
      (bitvector-set! 
       visibility 
       (row-major (player 'row) (player 'col) (cadr (array-dimensions (level 'squares)))) #t)
      (and-let* (((or (eqv? #t location)
		      (bitvector-ref visibility (row-major (car location) (cadr location) (level 'get-cols)))))
		 (text (if (procedure? text) (text visibility) text)))
	(message text #:x x #:y y)
	(player 'interrupt!)))))

(define clear-message (lambda () (message "") (addstr stdscr (format #f "~80,,a" "") #:y 0 #:x 0)))

(define* (player-select-direction #:optional (prompt "which direction? "))
  (message (format #f "~a (hjklyubn<esc>):" prompt))
  (let get-input ()
    (case (getch stdscr)
      ((#\j) 'down)
      ((#\k) 'up)
      ((#\;) 'none)
      ((#\h) 'left)
      ((#\l) 'right)
      ((#\y) 'up-left)
      ((#\u) 'up-right)
      ((#\b) 'down-left)
      ((#\n) 'down-right)
      ((#\esc) 'none)
      (else (get-input)))))

(define* (select-from-list lst disp prompt empty-message #:optional (count 1) (prev '()) #:key def)
  (clear-message)
  (refresh-level-view)
  (cond
   ((= (length lst) count) lst)
   ((< (length lst) count) (if def
			       (def)
			       (begin (message empty-message) #f)))
   (else (let ((choices (srfi-map (lambda (el letter row)
			     (message (format #f "~a - ~a" letter (disp el)) #:y (+ 1 row) #:x 5)
			     (cons letter el))       
			   lst all-letters (iota (length lst)))))
	 (message (format #f "~a" prompt))
	 (let ((choice (let pick-letter ((choice-letter (getch stdscr)))
			 (let ((choice (assq-ref choices choice-letter)))
			   (if choice 
			       choice 
			       (if (eq? choice-letter #\esc) 
				   (begin (message "Never mind.") #f) 
				   (pick-letter (getch stdscr))))))))
	   (if (and choice (> count 1))
	       (select-from-list (delete choice lst) disp prompt empty-message (- count 1) (cons choice prev))
	       (cons choice prev)))))))

(define player-select-square 
  (lambda (level location prompt)
    (message (format #f "~a (hjklyubn;<esc>):" prompt))
    (move stdscr (+ (car location) 1) (cadr location))
    (let ((input #\?))
      (while (not (eq? input #\;))
             (begin 
               (set! input (getch stdscr))
               (let ((new-location 
		      (make-new-location 
		       location
		       (case input
			 ((#\j) 'down)
			 ((#\k) 'up)
			 ((#\;) 'none)
			 ((#\h) 'left)
			 ((#\l) 'right)
			 ((#\y) 'up-left)
			 ((#\u) 'up-right)
			 ((#\b) 'down-left)
			 ((#\n) 'down-right)))))
                 (if (within-dimensions? new-location (level 'squares))
                   (set! location new-location))
                 (move stdscr (+ (car location) 1) (cadr location)))))
      location)))          

(define get-command 
  (lambda ()
    (flushinp)
    (let ((input (getch stdscr)))
      (case input
	((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) 
	 `(update-count ,(char-set->string (char-set input))))
	((#\esc) 'reset-count)
	((#\J) '(down long))
	((#\K) '(up long))
	((#\H) '(left long))
	((#\L) '(right long))
	((#\Y) '(up-left long))
	((#\U) '(up-right long))
	((#\B) '(down-left long))
	((#\N) '(down-right long))
	((#\>) 'down-stairs)
	((#\<) 'up-stairs)
	((#\o) 'open-door)
	((#\c) 'close-door)
	((#\/) 'follow-up-command)
	((#\j) 'down)
	((#\k) 'up)
	((#\h) 'left)
	((#\l) 'right)
	((#\y) 'up-left)
	((#\u) 'up-right)
	((#\t) 'throw)
	((#\b) 'down-left)
	((#\S) 'save)
	((#\n) 'down-right)
	((#\d) 'drop)
	((#\a) 'use)
	((#\;) 'look)
	((#\:) 'terminal)
	((#\i) 'inventory)
	((#\,) 'pick-up)
	((#\.) 'wait)
	((#\q) 'quit)
	(else 'unknown)))))

(define (color-for-memory-tile symbol)
  (case symbol
    ((down-stair up-stair) 1)
    (else 3)))

(define (char-for-symbol symbol)
  (case symbol
    ((at) (normal #\@))
    ((floor) (normal #\.))
    ((lit-floor) (normal #\.))
    ((blank) (normal #\ ))
    ((down-stair) (normal #\>))
    ((armor) (normal #\)))
    ((up-stair) (normal #\<))
    ((orc) (normal #\o))
    ((arthropod) (normal #\s))
    ((closed-door) (normal #\+))
    ((open-door) (normal #\'))
    ((mollusc) (normal #\m))
    ((human) (normal #\h))
    ((tool) (normal #\\))
    ((weapon) (normal #\|))
    ((wall) (normal #\#))))

(define refresh-level-view
  (let ((level #f)
	(agent #f))
    (lambda* (#:optional new-level new-agent)
	     (if new-level (set! level new-level))
	     (if new-agent (set! agent new-agent))
	     (if (and (agent 'player?) (not (agent #:def #f 'saved-command)))
		 (let ((squares (level 'squares))
		       (lighting (level 'lighting))
		       (visibility (level 'visibility))
		       (cols (level 'get-cols))
		       (player-memory (level 'player-memory)))
		   (for-each-index 
		    (level 'get-rows)
		    cols
		    (lambda (row col)
		      (move stdscr (+ row 1) col)
		      (if (bitvector-ref visibility (row-major row col cols))
			  (begin (let* ((ch (char-for-symbol ((array-ref squares row col) 'tile)))
					(lit-ch (bold (color 2 ch))))
				   (if (bitvector-ref lighting (row-major row col cols)) 
				       (addch stdscr lit-ch)
				       (addch stdscr (color 1 ch)))))
			  (let* ((memory (array-ref player-memory row col))
				 (memory-items (memory #:def #f 'items)))
			    (if (and memory-items (not (null? memory-items)))
				(addch stdscr (color 1 (char-for-symbol (assq-ref (car memory-items) 'symbol))))
				(if (memory #:def #f 'tile)
				    (addch 
				     stdscr 
				     (color 
				      (color-for-memory-tile (memory 'tile)) (char-for-symbol (memory 'tile))))
				    (addch stdscr (color 1 (normal #\ )))))))))
		   (for-each (lambda (thing)
			       (receive (row col)
				   (location-values (thing 'location))
				 (if (bitvector-ref 
				     visibility 
				     (row-major row col (level 'get-cols)))
				     (begin (move stdscr (+ row 1) col)
					   (addch stdscr (color 1 (char-for-symbol (thing 'symbol))))))))
			     (append (level 'items) (level 'agents)))
		   (addstr stdscr (format #f "Level ~2,,a" (+ (level 'depth) 1)) #:y 21 #:x 0)
		   (let ((player (level 'get-player)))
		     (move stdscr (+ (player 'row) 1) (player 'col)))
		   (refresh stdscr))))))

(define (player-select-item items prompt empty)
  (if (= (length items) 1)
      (car items)
      (begin
	(refresh-level-view)
	(display-items 
	 items 
	 prompt
	 empty (lambda (row)
		 (addstr stdscr "Which one? " #:y row #:x 5)
		 (echo!)
		 (let ((answer (getch stdscr)))
		   (noecho!)
		   (clear-message)
		   (find (lambda (item) (eq? (item 'letter) answer)) items)))))))

(define display-memory-items (lambda (items callback)
			       (refresh-level-view)
                               (begin (message "Items remembered here:")
                                      (for-each (lambda (item row) (message (format #f "     ~75,,a" (assq-ref item 'description)) #:y (+ row 1) #:x 0)) items (iota (length items)))
                                      (callback (+ (length items) 1)))))

(define display-items (lambda (items prompt empty callback)
			(refresh-level-view)
                        (if (null? items)
                          (begin (message empty)
                                 #f)
                          (begin (message prompt)
                                 (for-each (lambda (item row) (message (format #f "     ~75,,a" (item 'describe-inventory)) #:y (+ row 1) #:x 0)) items (iota (length items)))
                                 (callback (+ (length items) 1))))))

(define (display-clean-up) (clear stdscr) (refresh stdscr))
