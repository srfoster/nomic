#lang racket

;Nononono!!!
(provide (all-defined-out))

(require
  (submod nomic/gml/base games//relations)
  (submod nomic/gml/base VM))

(define (player #:name name)
  (thing #:name name
         'inventory '()))

(define lindsey
  (player #:name "Lindsey"))

(define stephen
  (player #:name "Stephen"))
  
(define players
  (thing #:name "Players"
         #:value (list stephen lindsey)))

(define state-checker
  (thing #:name "State Checker"
         'state-check (program
                       #:lang 'nomic/game/model
                       '(let ()
                          (map (lambda (t)
                                 (run 'state-check #:of t))
                               (all-things-in-play))))))

(define watchers
  (thing #:name "Watchers"
         #:value (list)))

(define (watcher-name w)
    (name #:of w))

(define (find-watcher-by-twitch-id id)
  (findf (lambda (w)
           (string=? id (watcher-name w)))
         (all-watchers)))

(define (all-watchers)
  (value #:of watchers))

(define (add-watcher! w)
  (define watcher-list (all-watchers))
  (define watcher-names-list (map watcher-name watcher-list))
  
  (define (already-watching? w)
    (define w-name (name #:of w))
    (member w-name watcher-names-list string=?))
  
  (when (not (already-watching? w))
   (redescribe! watchers 'value (cons w watcher-list))))

(define (has-name? n)
  (lambda (p)
    (string=? n (name #:of p))))

(define (find-player player-name)
  (define players (thing-named "Players" #:in the-game))
  (findf (has-name? player-name)
         (value #:of players)))

(define (add-to-inventory player-name thing)
  (define player (find-player player-name))
  (redescribe! player
               'inventory (cons thing (what-is 'inventory #:of player))))

(define (cast-spell player-name t)
  (add-to-inventory player-name t)
  (deplete-nexus-mana player-name (what-is 'mana #:of t)))

(define (nexus-for-player-name player-name)
  (define inventory (what-is 'inventory #:of (find-player player-name)))
  (findf (has-name? "Nexus") inventory))

(define (deplete-mana t mana)
  (redescribe! t 'mana (- (what-is 'mana #:of t) mana)))

(define (deplete-nexus-mana player-name mana)
  (define nexus (nexus-for-player-name player-name))
  (deplete-mana nexus mana))

(define (nexus #:color col #:mana mana)
  (thing #:name "Nexus"
         'color col
         'mana mana))

(define (sevarog #:mana mana)
  (thing #:name "Sevarog"
         'mana mana))

(define (clear-dead-target)
  (define target (my 'target))
  (when (is-dead? target)
    (redescribe! (my-self) 'target #f)))

(define (parasite #:target target #:mana [mana 1] #:regen [regen 30])
  (thing #:name "Parasite"
         'target target
         'mana mana
         'regen regen
         'leech (program
                 #:lang 'nomic/game/model
                 '(let ()
                    (define target (my 'target))
                    (define target-mana (what-is 'mana #:of target))
                    (define my-mana (my 'mana))
                    
                    (redescribe! target
                                 'mana
                                 (- target-mana (my 'regen)))

                    (redescribe! (my-self)
                                 'mana
                                 (+ my-mana (my 'regen)))))
         'state-check (program #:lang 'nomic/game/model
                               '(let ()
                                  (clear-dead-target)))))

(define (disenchantment #:target target #:mana mana)
  (thing #:name "Disenchantment"
         'target target
         'mana mana
         'disenchant (program
                      #:lang 'nomic/game/model
                      '(let ()
                         (define target (my 'target))
                         (define target-mana (what-is 'mana #:of target))
                         (define my-mana (my 'mana))
                    
                         (redescribe! target
                                      'mana
                                      (- target-mana my-mana))
                         ;sets its own mana to 0 so it gets destroyed
                         (redescribe! (my-self)
                                      'mana
                                      0)))
         'state-check (program #:lang 'nomic/game/model
                               '(let ()
                                  (clear-dead-target)
                                  (if-targetless-move-self-to-graveyard)))
         ))

(define (if-targetless-move-self-to-graveyard)
  (when (targetless? (my-self))
    (move-to-graveyard (my-self))))

(define (targetless? t)
  (not (what-is 'target #:of t)))

(define (move-to-graveyard t)
  (redescribe! graveyard 'value (cons t (value #:of graveyard)))
  (define player (player-controlling t))
  (redescribe! player 'inventory (remove t (inventory #:of player))))

(define (inventory #:of p)
  (what-is 'inventory #:of p))

(define (player-controlling t)
  (define ps (value #:of players))
  (findf (lambda (p)
           (in-player-inventory? t p)) ps))

(define (in-player-inventory? t p)
  (member t (inventory #:of p)))

(define (is-dead? t)
  (< (what-is 'mana #:of t) 0))

(define graveyard (thing #:name "Graveyard"
                   'value '()))

(define reaper
  (thing #:name "Reaper"
         'reap (program #:lang 'nomic/game/model
                        '(let ()
                           (define dead-things
                             (filter is-dead?
                                     (all-things-in-play)))
                           (map move-to-graveyard dead-things))
                )))

(define (all-things-in-play)
  (define ps (value #:of players))
  (flatten
   (map (lambda (p)
         (inventory #:of p))
       ps)))

(define (find-by-name-in-inventory name player-name)
  (define player (find-player player-name))
  (define inventory (what-is 'inventory #:of player))
  (findf (has-name? name)
   inventory))

(define the-game
 (put-in (new-game)
         reaper
         state-checker
         graveyard
         players
         watchers
         ))

(define (mana-of t)
  (what-is 'mana #:of t))

(define (sample-game) 
  (local-require rackunit)
  (define lindsey-nexus (nexus #:mana 100 #:color 'green))
  (define stephen-nexus (nexus #:mana 100 #:color 'pink))
  (add-to-inventory "Lindsey" lindsey-nexus)
  (add-to-inventory "Stephen" stephen-nexus)
  
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
  )

;(run 'reap #:of reaper)

;TODO: Reeep things whose target is dead...
  ; Or any children?

(module+ main
  (sample-game)
  (require nomic/gml/server
           racket/sandbox
           web-server/http/bindings)

  (on-twitch-spell
   (lambda (r)
     (safe-run-spell (extract-twitch-id r) (extract-spell r))
     ))


  (define (safe-run-spell twitch-id spell-string)
    (displayln "TWITCH: safe-run-spell")
    (displayln spell-string)
    
    (define thing-description
      (twitch-eval twitch-id spell-string))

    (displayln thing-description)

    (cond
      [(eq? 'Watcher (hash-ref thing-description 'type))
       (add-watcher! ;(thing #:name twitch-id #:type 'Watcher)
        (thingify thing-description))]
      [(eq? 'WatcherDescribe (hash-ref thing-description 'type))
       (redescribe! (find-watcher-by-twitch-id twitch-id)
                    (hash-ref thing-description 'key)
                    (hash-ref thing-description 'value))]
      [else
       (error "What was that???")]))

  (define (twitch-eval twitch-id spell-string)
    (safe-evaluator
     `(with-twitch-id ,twitch-id
        ,(read (open-input-string spell-string)))))

  (require nomic/new-twitch)
  (define safe-evaluator
    (call-with-trusted-sandbox-configuration
     (lambda ()
       (make-evaluator
        'nomic/new-twitch
        ))))

  (define (extract-spell r)
    (extract-binding/single
     'spell
     (request-bindings r)))

  (define (extract-twitch-id r)
    (extract-binding/single
     'twitch-id
     (request-bindings r)))
  
  (thread (thunk (serve-model the-game)))


  (require nomic/gml/frontend)
  (thread (thunk (serve-renderer the-game))))



