#lang racket

(require (submod nomic/game/model moves))

(local-require rackunit)

(start-game-with-players "Runi" "Laurond" "Woogachaka")
(place-nexus #:mana 100 #:color 'green)
(end-turn!)
(check-eq? (current-player-name) "Laurond")
(place-nexus #:mana 100 #:color 'blue)
(end-turn!)
(check-eq? (current-player-name) "Woogachaka")
(place-nexus #:mana 100 #:color 'green)
(end-turn!)
(check-eq? (current-player-name) "Runi")
 
; TODO: Put in check expects

#|
(cast-spell "Lindsey"
            (sevarog  #:mana 1))
  
(check-eq? (mana-of lindsey-nexus) 99)
(check-eq? (length (value #:of graveyard)) 0)
  
(cast-spell "Stephen"
            (parasite #:target (find-by-name-in-inventory "Sevarog" "Lindsey")
                      #:mana 1
                      #:regen 200 ;super strong for testing purposes
                      ))
(check-eq? (mana-of stephen-nexus) 99)

(define my-disenchantment
  (disenchantment #:mana 50
                  #:target (find-by-name-in-inventory "Parasite" "Stephen")))
(cast-spell "Lindsey"
            my-disenchantment)
(check-eq? (mana-of lindsey-nexus) 49)
(run 'disenchant #:of (find-by-name-in-inventory "Disenchantment" "Lindsey"))
(check-pred is-dead? (what-is 'target #:of my-disenchantment)
            "The target of my disenchantment should be dead!")

(void (run 'reap #:of reaper))
(check-eq? (length (value #:of graveyard)) 1)
(void (run 'state-check #:of state-checker))
(check-eq? (length (value #:of graveyard)) 2)
|#