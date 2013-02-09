;; Updated!
(define-module (stay-alive weapon)
  #:use-module (shelf shelf)
  #:use-module (srfi srfi-2)
  #:use-module (stay-alive item)
  #:use-module (stay-alive util)
  #:export (Weapon Longsword Cutlass))

(define-objects Weapon
  ((symbol 'weapon)
   (can-wield? #t)
   (weapon? #t)	 
   (use (method (level agent)
	  (if ($ this 'wielded? #:def #f) 
	      ($ this 'unwield level agent)
	      ($ this 'wield level agent)))))
  ((Sword
    ((wield-requires (h (quantity 1)
			(pred (method (part) 
				(eq? ($ part 'name) 'hand)))))
     (melee-verbs '(to-slice to-cut to-stab)))
    ((TwoHandedSword  
      ((name "two-handed sword")
       (wield-requires (h (quantity 2)))))
     (Longsword
      ((name "longsword that is awesome")
       (attack `((slice 
		  (verb 'to-slice)
		  (to-hit ,(percent 5))
		  (damage 4))
		 (sharpness (,(percent 10) 5))))
       (describe-long "A massive, sharp, and surprisingly light \
sword that seems to be meant for two hands.")))
     (Cutlass
      ((name "cutlass")
       (slicing `(,(percent 8) 4))
       (heft `(,(percent 3) 2))
       (sharpness `(,(percent 8) 4))
       (describe-long "A short, broad sword with a slight curve. \
Picking it up makes you feel like a pirate.")))))))

(extend Item (list Weapon))
