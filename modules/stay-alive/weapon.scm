(define-module (stay-alive weapon)
  #:use-module (shelf shelf)
  #:use-module (srfi srfi-2)
  #:use-module (stay-alive item)
  #:export (Weapon Longsword Cutlass))

(define-objects Weapon
  ((symbol 'weapon)
   (can-wield? #t)
   (weapon? #t)	 
   (use (method (level agent)
	  (if (this 'wielded? #:def #f) 
	      (this 'unwield level agent)
	      (this 'wield level agent)))))
  ((Sword 
    ((wield-requires (h (quantity 1)
			(pred (method (part) 
				(eq? (part 'name) 'hand))))))
    ((TwoHandedSword  
      ((wield-requires (h (quantity 2)))))
     (Longsword
      ((name "longsword")
       (describe-long "A massive, sharp, and surprisingly light sword that seems to be meant for two hands.")))
     (Cutlass
      ((name "cutlass")
       (describe-long "A short, broad sword with a slight curve. Picking it up makes you feel like a pirate.")))))))

(extend Item (list Weapon))