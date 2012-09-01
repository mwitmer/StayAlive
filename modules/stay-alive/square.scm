(define-module (stay-alive square)
  #:use-module (shelf shelf)
  #:use-module (stay-alive ncurses-interface)
  #:export (Square Floor RoomFloor OpenDoor DownStair UpStair RoomWall ClosedDoor Wall))

(define-objects Square  
  ((go-down (method (agent level) (if (agent 'player?) (message "no down stairs here")) #f))
   (go-up (method (agent level) (if (agent 'player?) (message "no up stairs here")) #f)))
  ((Floor
    ((tile 'floor)
     (weight 10)
     (description "a bare spot of floor")
     (opaque? #f)
     (stops-projectiles? #f)
     (can-agent-enter? (method (agent) #t)))
    ((RoomFloor 
      ((room-floor? #t)))
     (OpenDoor
      ((tile 'open-door)
       (open-door? #t)
       (description "an open door")))
     (DownStair
      ((tile 'down-stair)
       (down-stairs? #t)
       (description "a twisting staircase down into the shadows")
       (go-down 
	(method (agent level) 
	  (leave-level agent level 'descending-stairs (+ (agent 'depth) 1) "you go down the stairs") #t))))
     (UpStair
      ((tile 'up-stair)
       (up-stairs? #t)
       (description "a twisting staircase up into the darkness")
       (go-up 
	(method (agent level) 
	  (leave-level agent level 'ascending-stairs (- (agent 'depth) 1) "you go up the stairs") #t))))))
   (Wall
    ((tile 'wall)
     (description "a solid wall")
     (weight 1000)
     (opaque? #t)
     (stops-projectiles? #t)
     (can-agent-enter? #f))
    ((RoomWall ())
     (ClosedDoor
      ((tile 'closed-door)				      
       (weight 50)
       (closed-door? #t)
       (description "a tightly closed door")))))))

(define (leave-level agent level status new-depth msg)
  (if (agent 'player?) (message msg) (display-refresh))
  (set! (agent 'status) status)
  (set! (agent 'depth) new-depth)
  (agent 'on-leave-square level (agent 'location))
  ((level 'agent-queue) 'remove! agent))