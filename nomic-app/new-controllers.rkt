#lang at-exp web-server

(provide start-server)

(require racket/sandbox
         racket-react/server
         json
         (submod nomic/gml/base games//relations)
         (submod nomic/gml/base VM))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define the-game
  (new-game))

(define (set-the-game! g)
  (set! the-game g))

(define (x->json x)
  (cond [(symbol? x)  (~a x)]
        [(number? x)  x]
        [(string? x)  x]
        [(boolean? x) x]
        [(hash? x)
         (let ()
           (define new-x (hash))
           (for ([k (hash-keys x)])
             (set! new-x (hash-set new-x k (x->json (hash-ref x k)))))
           new-x)]
        [(void? x)  "null"]
        [(thing? x) (cond
                      [(program? x)
                       (hash 'name (what-is 'name #:of x)
                             'type "Program"
                             'value (~v (value #:of x)))
                       ]
                      [else (thing->json x)])]
        [(list? x) (map x->json x)]
        [(game? x) (game->json x)]
        [else (~a x)]))

(define (thing->json t)
  (define h (describe-thing t))
  (define ret (hash))

  (for ([k (hash-keys h)])
    (set! ret (hash-set ret k (x->json (hash-ref h k)))))
  
  ret)

(define (game->json g)
  (hash
   'type "game"
   'things (map thing->json (things-in g))))

(define (run-script)
 
  (define script (arg 'script))

  (define evaluation
    (eval (read (open-input-string script))
          ns))
 
  (welcome)
  )

(define (welcome)
  (with-embeds
    (response/json/cors
      (hash
        'type "game-state"

        'refresh
        (embed welcome)

        'gameState
        (game->json the-game)

        'runScript
        (embed run-script)
        ))))

(define-values (do-routing url)
  (dispatch-rules
    [("top")
     (start
      (lambda (r)
        (welcome)))]
    [("twitch-spell")
     (lambda (r)
       (safe-run-spell (extract-twitch-id r) (extract-spell r))
       (with-embeds
           (response/json/cors
            (hash))))]))

#;
(define (thingify td)
  (define t (thing #:name "Dummy"))
  (for ([k (hash-keys td)])
    (redescribe! t k (hash-ref td k)))
  t)

(define (safe-run-spell twitch-id spell-string)
  (define thing-description
    (twitch-eval twitch-id spell-string))

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

(define (start-server)
  (serve/servlet do-routing
                 #:port 8081
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 #:stateless? #t))



(define (player #:name name)
  (thing #:name name
         'inventory '()))

(define stephen
  (player #:name "Stephen"))

(define lindsey
  (player #:name "Lindsey"))
  
(define players
  (thing #:name "Players"
         #:value (list stephen lindsey)))

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

(define (nexus #:color col #:mana mana)
  (thing #:name "Nexus"
         'color col
         'mana mana))

(define (sevarog #:mana mana)
  (thing #:name "Sevarog"
         'mana mana))

(define (parasite #:target target #:mana [mana 1] #:regen [regen 30])
  (thing #:name "Parasite"
         'target target
         'mana mana
         'regen regen
         'leech (program
                 '(let ()
                    (define target (my 'target))
                    (define target-mana (what-is 'mana #:of target))
                    (define my-mana (my 'mana))
                    
                    (redescribe! target
                     'mana
                     (- target-mana (my 'regen)))

                    (redescribe! (my-self)
                     'mana
                     (+ my-mana (my 'regen)))))))

(define (disenchantment #:target target #:mana mana)
  (thing #:name "Disenchantment"
         'target target
         'mana mana
         'disenchant (program
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
                                      0)
                         ))))

(define (move-to-graveyard t)
  (redescribe! graveyard 'value (cons t (value #:of graveyard)))
  (define player (player-controlling t))
  (redescribe! player 'inventory (remove t (inventory #:of player))))

(define (inventory #:of p)
  (what-is 'inventory #:of p))

(define (player-controlling t)
  (define players (value #:of players))
  (findf (lambda (p)
           (in-player-inventory? t p)) players))

(define (in-player-inventory? t p)
  (member t (inventory #:of p)))

(define (is-dead? t)
  (< (what-is 'mana #:of t) 0))

(define graveyard (thing #:name "Graveyard"
                   'value '()))


(provide is-dead? move-to-graveyard)
(define reaper
  (thing #:name "Reaper"
         'reap (program #:lang 'nomic/nomic-app/new-controllers
                '(let ()
                   (define dead-things
                     (filter is-dead?
                             (all-things-in-play)))
                   (map move-to-graveyard dead-things)))))

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

(set-the-game!
 (put-in (new-game)
         reaper
         graveyard
         players
         watchers
         ))


;;;;

(add-to-inventory "Lindsey" (nexus #:mana 100 #:color 'green))
(add-to-inventory "Stephen"
                  (parasite #:target (find-by-name-in-inventory "Nexus" "Lindsey")
                            #:mana 1
                            #:regen 300
                            ))
(run 'leech #:of (find-by-name-in-inventory "Parasite" "Stephen"))
;(run 'reap #:of reaper)

;;;;


(describe-things-in the-game)
;(require codespells)
;(codespells-server-port 7998)
;(unreal-server-port 7999)
 
(thread start-server)

(module+ main

  )