;; Updated!
(define-module (stay-alive delta-queue)
  #:use-module (shelf shelf)
  #:use-module (stay-alive util)
  #:export (DeltaQueue))

(define-object DeltaQueue 
  (members '())
  (get-members (method () (map (lambda (el) (car el)) ($ this 'members))))
  (remove! 
   (method (item)
     (set! ($ this 'members) 
	   (filter (lambda (mem) (not (eq? (car mem) item))) ($ this 'members)))))
  (enqueue! 
   (method (item delta) 
     (set! ($ this 'members) 
	   (insert-ordered 
	    (list item delta) 
	    (lambda (val el)
	      (if (< (cadr val) (cadr el))
		  (begin (list-set! el 1 (- (cadr el) (cadr val))) #t)
		  (begin (list-set! val 1 (- (cadr val) (cadr el))) #f)))
	    ($ this 'members)))))
  (dequeue! 
   (method ()
     (let* ((members ($ this 'members))
	    (result (if (null? members)
			*unspecified*
			(car members))))
       (set! ($ this 'members) (cdr members))
       (values (car result) (cadr result))))))
