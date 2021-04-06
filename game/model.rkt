#lang racket

;Nononono!!!
(provide (all-defined-out))

(require
  (submod nomic/gml/base games//relations)
  (submod nomic/gml/base games//graphs)
  (submod nomic/gml/base VM))

;;GRID

(define-values (board-g tiles) (grid-graph 8 8))

(define chessboard-battlefield 
  (thing #:name "Battlefield"
         #:value tiles))

(define (put-on-battlefield p x y)
  (define t (get-tile/xy board-g x y))

  (enter-tile board-g p t))

;;; STATE CHECKER
(define state-checker
  (thing #:name "State Checker"
         'state-check (program
                       #:lang 'nomic/game/model
                       '(let ()
                          (map (lambda (t)
                                 (run 'state-check #:of t))
                               (all-things-in-play))))))

;;; WATCHERS
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


;;;UTIL
(define (has-name? n)
  (lambda (p)
    (string=? n (name #:of p))))

(define (has-type? n)
  (lambda (p)
    (eq? n (type #:of p))))

(define (find-by-name-in-inventory name player-name)
  (define player (find-player player-name))
  (define inventory (what-is 'inventory #:of player))
  (findf (has-name? name)
         inventory))

(define (find-by-type-in-inventory type player-name)
  (define player (find-player player-name))
  (define inventory (what-is 'inventory #:of player))
  (findf (has-type? type)
         inventory))

(define (all-things-in-play)
  (define ps (value #:of players))
  (flatten
   (map (lambda (p)
          (inventory #:of p))
        ps)))

(define (things-in-play-for p)
  (inventory #:of p))

(define (set-destination-of t1 #:to t2)
  (redescribe! t1 'destination t2))

(define (destination-of t1)
  (what-is 'destination
           #:of t1
           #:fallback #f))

(define (find player-name type)
  (findf (has-type? type)
         (things-in-play-for (find-player player-name))))

;;;PLAYERS

(define the-game (make-parameter (new-game)))

(define (player #:name name)
  (thing #:name name
         #:type 'Player
         'inventory '()))

(define players
  (thing #:name "Players"
         #:value (list)
         'current-player 0))

(define (name->player n)
  (player #:name n))

(define (next-player)
  (redescribe! players 'current-player
               (modulo
                (add1 (what-is 'current-player #:of players))
                (length (value #:of players)))))

(define (current-player)
  (list-ref (value #:of players) (what-is 'current-player #:of players)))

(define (current-player-name)
  (name #:of (current-player)))

(define (find-player player-name)
  (define players (thing-named "Players" #:in (the-game)))
  (findf (has-name? player-name)
         (value #:of players)))

(define (add-to-inventory player-name thing)
  (define player (find-player player-name))
  (redescribe! player
               'inventory (cons thing (what-is 'inventory #:of player))))



(define (deplete-mana t mana)
  (redescribe! t 'mana (- (what-is 'mana #:of t) mana)))

;;; NEXUS
(define (nexus-for-player-name player-name)
  (define inventory (what-is 'inventory #:of (find-player player-name)))
  (findf (has-name? "Nexus") inventory))

(define (deplete-nexus-mana player-name mana)
  (define nexus (nexus-for-player-name player-name))
  (deplete-mana nexus mana))

(define (nexus #:color col #:mana mana)
  (thing #:name "Nexus"
         #:type 'Nexus 
         'color col
         'mana mana))

;;; SEVAROG
(define (sevarog #:mana mana)
  (thing #:name "Sevarog"
         #:type 'Sevarog
         'destination #f
         'mana mana))

;;; PARASITE
(define (parasite #:target target #:mana [mana 1] #:regen [regen 30])
  (thing #:name "Parasite"
         #:type 'Parasite
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

;;DISENCHANTMENT
(define (disenchantment #:target target #:mana mana)
  (thing #:name "Disenchantment"
         #:type 'Disenchantment
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

;;;GRAVEYARD
(define (if-targetless-move-self-to-graveyard)
  (when (targetless? (my-self))
    (move-to-graveyard (my-self))))

(define (targetless? t)
  (not (what-is 'target #:of t)))

(define (clear-dead-target)
  (define target (my 'target))
  (when (is-dead? target)
    (redescribe! (my-self) 'target #f)))

(define (move-to-graveyard t)
  (redescribe! graveyard 'value (cons t (value #:of graveyard)))
  (define player (player-controlling t))
  (redescribe! player 'inventory (remove t (inventory #:of player))))

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

;; INVENTORY
(define (inventory #:of p)
  (what-is 'inventory #:of p))

(define (player-controlling t)
  (define ps (value #:of players))
  (findf (lambda (p)
           (in-player-inventory? t p)) ps))

(define (in-player-inventory? t p)
  (member t (inventory #:of p)))









(define (mana-of t)
  (what-is 'mana #:of t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ moves
  (provide
   ;query game state
   things-in-play-for
   current-player
   current-player-name
   has-type?
   has-name?
   (rename-out [find-player player-named])
   set-destination-of
   destination-of
   find

   ;changes game state
   cast-spell
   place-nexus
   start-game-with-players
   end-turn!
   sevarog
   parasite
   disenchantment

   put-on-battlefield
   chessboard-battlefield
   )
  
  (define (cast-spell t)
    (add-to-inventory (current-player-name) t)
    (deplete-nexus-mana (current-player-name) (what-is 'mana #:of t)))
  
  (define (start-game-with-players #:map [the-map chessboard-battlefield] . names)
    (redescribe! players 'value (map name->player names))

    (the-game (put-in (the-game)
                      the-map
                      players)))
  
  (define (place-nexus #:mana mana #:color color)
    (when (> mana 100) ;This is where you would enforce nexus properties
      (error "You can't have a nexus with more than 100 mana! Cheater!"))
    (add-to-inventory (current-player-name) (nexus #:mana mana #:color color)))
  
  (define (end-turn!)
    (next-player))
  )



;(run 'reap #:of reaper)

;TODO: Reeep things whose target is dead...
; Or any children?

(provide serve-game)

(require nomic/gml/server
         racket/sandbox
         web-server/http/bindings)
(require nomic/new-twitch)
(require nomic/gml/frontend)

(define (serve-game [post-process-twitch (thunk* (error "What was that!?"))]
                    [twitch-language 'nomic/new-twitch])
  
  (the-game
    (put-in (the-game)
            ;reaper
            ;state-checker
            ;graveyard
            ;players
            watchers

            ;chessboard-battlefield
            ))

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
       (post-process-twitch thing-description)]))

  (define (twitch-eval twitch-id spell-string)
    (safe-evaluator
     `(with-twitch-id ,twitch-id
        ,(read (open-input-string spell-string)))))
  
  (define safe-evaluator
    (call-with-trusted-sandbox-configuration
     (lambda ()
       (make-evaluator
        twitch-language
        ))))

  (define (extract-spell r)
    (extract-binding/single
     'spell
     (request-bindings r)))

  (define (extract-twitch-id r)
    (extract-binding/single
     'twitch-id
     (request-bindings r)))
  
  (thread (thunk (serve-model (the-game))))


 
  (thread (thunk (serve-renderer (the-game)))))



