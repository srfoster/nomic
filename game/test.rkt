#lang racket

(require (submod nomic/game/model moves)
         (prefix-in private: (submod nomic/gml/base games//relations)))

(local-require rackunit)

(start-game-with-players "Runi" "Laurond" "Woogachaka" "Kenzo")
(place-nexus #:mana 100 #:color 'green)
(end-turn!)

(check-eq? (current-player-name) "Laurond")
(place-nexus #:mana 100 #:color 'blue)
(end-turn!)

(check-eq? (current-player-name) "Woogachaka")
(place-nexus #:mana 100 #:color 'green)
(end-turn!)

(check-eq? (current-player-name) "Kenzo")
(place-nexus #:mana 100 #:color 'orange)
(end-turn!)

(check-eq? (current-player-name) "Runi")
(check-eq? 1
           (length (things-in-play-for (current-player))))
(cast-spell (sevarog  #:mana 30))
(set-destination-of (find "Runi" 'Sevarog)
                    #:to (find "Laurond" 'Nexus))
(check-eq? (destination-of (find "Runi" 'Sevarog))
           (find "Laurond" 'Nexus))
(check-eq? 2
           (length (things-in-play-for (current-player))))
(end-turn!)

(check-eq? (current-player-name) "Laurond")
(check-eq? 1
           (length (things-in-play-for (current-player))))
(cast-spell (parasite #:target (find "Runi" 'Sevarog)
                      #:regen 30
                      #:mana 1))
(check-eq? 2
           (length (things-in-play-for (current-player))))
(end-turn!)

(check-eq? (current-player-name) "Woogachaka")
(check-eq? 1
           (length (things-in-play-for (current-player))))
(cast-spell (sevarog  #:mana 30))
(end-turn!)

(check-eq? (current-player-name) "Kenzo")
(cast-spell (parasite #:target (find "Woogachaka" 'Sevarog)
                      #:regen 15
                      #:mana 1))
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