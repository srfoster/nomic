#lang at-exp web-server

(provide start-server)

(require racket/sandbox
         racket-react/server
         codespells-server/unreal-js/unreal-client
         json
         "../lang.rkt")

(define command-history '())

(define (welcome)
  (with-embeds
    (response/json/cors
      (hash
        'type "game-state"

        'commandHistory
        command-history

        'actionLog
        (map log-item->json (game-action-log the-game))
        
        'gameState
        (hash
         'changePlayer (embed toggle-player)
         'watchers (map watcher->json (game-watchers the-game))
         'players (map player->json (game-players the-game))

         #;(list (hash 'name "Stephen"
                              'manaConstructs
                              (map mana-construct->json
                                   (filter (belongs-to "Stephen") (game-mana-constructs the-game))))
                        (hash 'name "Lindsey"
                              'manaConstructs
                              (map mana-construct->json
                                   (filter (belongs-to "Lindsey") (game-mana-constructs the-game)))))
         'currentPlayer (player->json (game-current-player the-game))
         ;'manaConstructs
         ;(map mana-construct->json all-mana-constructs)
         )

        'runScript
        (embed run-script)

        'refresh
        (embed welcome)
        ))))

(define (player->json p)
  (hash 'name (player-name p)
        'manaConstructs
        (map mana-construct->json
             (filter (belongs-to (player-name p)) (game-mana-constructs the-game)))))

(define (toggle-player)
  (end-turn!)
  (welcome))

(define (watcher->json w)
  (define watcher-id (twitch-eval "" "watcher-id"))
  (hash 'id (watcher-id w)
        'mana (watcher-mana w)
        ))

(define (belongs-to name)
  (lambda (mc)
    (string=? name (mana-construct-owner mc))))

(define (run-script)
  (displayln "run-script")

  (define script (arg 'script))

  (displayln "script")
  (displayln script)

  (set! command-history
        (cons script command-history))

  (define evaluation
   (eval (read (open-input-string script))
         (module->namespace 'nomic/lang)))

  (displayln "evaluation")
  (displayln evaluation)

  (thread
   (thunk
    (unreal-eval-js evaluation)))
  
  
  (welcome)
  )

(define (mana-construct->json mc)
  (hash 'id (mana-construct-id mc)
        'mana (mana-construct-mana mc)
        'owner (mana-construct-owner mc)
        'name (mana-construct-name mc)))

(define (log-item->json log-item)
  (hash 'type (symbol->string (action-type log-item))
        'owner (action-owner log-item)
        'turn (action-turn log-item)))

(define-values (do-routing url)
  (dispatch-rules
    [("top")
     (start
      (lambda (r)
        (welcome)))]
    [("twitch-spell")
     (lambda (r)
       (displayln "Running from Twitch...")
       (safe-run-spell (extract-twitch-id r) (extract-spell r))
       (with-embeds
           (response/json/cors
            (hash))))]))

(require (only-in codespells at))
(require nomic/twitch
        ; ycombinator-demos/chess
         )

(define (safe-run-spell twitch-id spell-string)

  (define evaluated-spell
    (twitch-eval twitch-id spell-string))


  ;Define side-effects that are allowed via twitch commands

  ;1) Allowed spells get executed in-game.  nomic/twitch functions return unreal-js-fragments
  (when (or (string? evaluated-spell)
            (procedure? evaluated-spell)
            (unreal-js-fragment? evaluated-spell))
    (unreal-eval-js
     evaluated-spell))


  ;2) Allowed changes to game state get executed, e.g.

  ;The take-action function will decide if this
  ; current action is allowed by the current user at the current time

  (when (action? evaluated-spell)
    (take-action evaluated-spell)))

;(safe-run-spell "Hexagon" "(watch)")


(define safe-evaluator
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (make-evaluator
      ;'ycombinator-demos/chess
      'nomic/twitch
      ))))

(define (twitch-eval twitch-id spell-string)
  (safe-evaluator
   `(with-twitch-id ,twitch-id
      ,(read (open-input-string spell-string)))))

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

(module+ main
  (require codespells)

  (codespells-server-port 7998)
  (unreal-server-port 7999)
  (start-game "Stephen" "Lindsey")
  (thread start-server)
  )
