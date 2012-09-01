(define-module (stay-alive armor)
  #:use-module (shelf shelf)
  #:use-module (stay-alive item)
  #:export (Armor
	    Buckler
	    LeatherArmor
	    Gauntlets))

(define-objects Armor 
  ((symbol 'armor)
   (armor #t)
   (use (method (level agent)
	  (if (this 'worn? #:def #f)
	      (this 'take-off level agent)
	      (this 'put-on level agent))))
   (can-wear? #t))
  ((Shield 
    ((wear-requires 
      (h (also-wield? 'hand)
	 (quantity 1)
	 (pred (method (part) (eq? (part 'name) 'arm))))))
    ((Buckler
      ((name "buckler")
       (describe-long "A handy little shield that can block a sword blow but won't do you much good versus slings and arrows.")))))
   (BodyArmor 
    ((wear-requires (h (quantity 1)
		       (pred (method (part) (eq? (part 'name) 'torso))))))
    ((LeatherArmor 
      ((name '("suit of leather armor" "suits of leather armor"))
       (describe-long "A thing layer of animal skin that doesn't seem to protect you from much.")))))

   (BaseGloves
    ((wear-requires (h (quantity 2)
		       (pred (method (part) (eq? (part 'name) 'hand))))))
    ((Gauntlets
      ((name '("pair of gauntlets" "pairs of gauntlets"))
       (describe-long "A shiny pair of metal gloves with a cold, snug fit.")))))))

(extend Item (list Armor))