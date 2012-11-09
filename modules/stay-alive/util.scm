(define-module (stay-alive util)
  #:use-module (srfi srfi-1)
  #:export (for-each-index
	    make-new-location
	    all-directions
	    within-dimensions?
	    inside-dimensions?
	    locations-adjacent?
	    list-ref-random
	    random-match
	    percent
	    all-directions
	    pred-and
	    pred-not
	    pred-ident
	    pred-proc-eq
	    pred-eq
	    proc-begin
	    all-letters
	    insert-ordered
	    line-for-each
	    search-string
	    fold-each-index
	    bitvector-foreach-row-major
	    increase-odds
	    decrease-odds
	    e
	    percent
	    location-values
	    inverse-row-major
	    row-major))

(define e 2.71828182845894523536)

(define (percent n)
  (/ n 100))

(define (increase-odds odds multiplier)
  (- 1 (* (- 1 odds) (- 1 multiplier))))

(define (decrease-odds odds multiplier)
  (* odds multiplier))

(define (inverse-row-major n cols)
  (list (quotient n cols) (remainder n cols)))

(define (bitvector-foreach-row-major proc bv cols)
  (for-each (lambda (n) 
	      (if (bitvector-ref bv n) 
		  (apply proc (inverse-row-major n cols)))) 
	    (iota (bitvector-length bv))))

(define all-directions '(up down left right up-right up-left down-right down-left))
(define (search-string str test-proc apply-proc fail-proc)
		       (let search-recur ((idx 0))
			 (if (test-proc str idx)
			     (apply-proc str idx)
			     (if (< idx (- (string-length str) 1))
				 (search-recur (+ idx 1))
				 (fail-proc str)))))

(define all-letters 
  (map! (lambda (ch) (if (char-upper-case? ch) (char-downcase ch) (char-upcase ch)))
	(char-set->list (char-set-intersection char-set:letter char-set:ascii))))

(define (random-match proc lst)
  (if (null? lst) #f
      (let* ((el (list-ref-random lst))
	     (res (proc el)))
       (if res res (random-match (delete el lst) proc)))))

(define (list-ref-random l) (list-ref l (random (length l))))

(define (pred-ident pred) (lambda (el) pred))
(define (pred-and pred1 pred2) (lambda (el) (and (pred1 el) (pred2 el))))
(define (pred-not pred1) (lambda (el) (not (pred1 el))))
(define (pred-eq obj) (lambda (el) (eq? el obj)))
(define (pred-proc-eq obj proc) (lambda (el) (eq? (proc el) obj)))

(define proc-begin (lambda args 
		     (lambda (obj)
		       (for-each (lambda (arg) (arg obj)) args))))

(define (locations-adjacent? loc1 loc2)
  (and (<= (abs (- (car loc1) (car loc2))) 1)
       (<= (abs (- (cadr loc1) (cadr loc2))) 1)))

(define (for-each-index rows cols proc)
  (do ((row 0 (1+ row)))
    ((= row rows))
    (do ((col 0 (1+ col)))
      ((= col cols))
      (proc row col))))

(define (row-major row col cols)
  (+ (* row cols) col)) 

(define inside-dimensions?
  (lambda (location squares)
    (let ((dimensions (array-dimensions squares))
          (row (car location))
          (col (cadr location)))
      (and (> row 0) (> col 0) (< col (- (cadr dimensions) 1)) (< row (- (car dimensions) 1))))))

(define within-dimensions? 
  (lambda (location squares)
    (let ((dimensions (array-dimensions squares))
          (row (car location))
          (col (cadr location)))
      (and (>= row 0) (>= col 0) (< col (cadr dimensions)) (< row (car dimensions))))))

(define make-new-location 
  (lambda (location direction)
    (let ((row (car location))
          (col (cadr location)))
      (case direction
        ((up) (list (- row 1) col))
        ((down) (list (+ row 1) col))
        ((left) (list row (- col 1)))
        ((right) (list row (+ col 1)))
        ((none) (list row col))
        ((up-left) (list (- row 1) (- col 1)))
        ((up-right) (list (- row 1) (+ col 1)))
        ((down-left) (list (+ row 1) (- col 1)))
        ((down-right) (list (+ row 1) (+ col 1)))))))

(define (insert-ordered val compare lst)
  (let insert-ordered-recur ((val val) (compare compare) (lst lst) (accum '()))
    (if (null? lst) (append accum (list val))
      (if (compare val (car lst)) 
        (append accum (cons val lst))
        (insert-ordered-recur val compare (cdr lst) (append accum (list (car lst))))))))

(define (fold-each-index rows cols proc start)
  (do ((row 0 (1+ row))
       (final start (do ((col 0 (1+ col)) 
                         (final-col final (proc row col final-col)))
                      ((= col cols) final-col))))
    ((= row rows) final)))

(define (line-for-each origin target proc)
  (letrec* ((dx (abs (- (cadr target) (cadr origin))))
            (dy (abs (- (car target) (car origin))))
            (sx (if (< (cadr origin) (cadr target)) 1 -1))
            (sy (if (< (car origin) (car target)) 1 -1))
            (start-err (- dx dy))
            (plot (lambda (current previous err first? distance)
                    (if (or first? (not (proc current previous distance)))
                      (let* ((e2 (* 2 err))
                             (e2gndy (> e2 (- dy)))
                             (e2ldx (< e2 dx)))
                        (plot (list
                                (if e2ldx (+ (car current) sy) (car current))
                                (if e2gndy (+ (cadr current) sx) (cadr current)))
                              current
                              (+ err (if e2gndy (- dy) 0) (if e2ldx dx 0))
                              #f
                              (+ 1 distance)))))))
           (plot origin #f start-err #t 0)))

(define (location-values location)
  (values (car location) (cadr location)))
