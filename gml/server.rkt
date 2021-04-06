#lang at-exp web-server

(provide serve-model

         ;Gross.
         on-twitch-spell)

(require racket/sandbox
         racket-react/server
         codespells-server/spell-execution
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
                      #;
                      [(program? x)
                       (let ([p (thing->json x)])
                         (hash-set p 'RunMe "EMBED??"))
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
 
  (welcome))

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

(define on-twitch-spell (make-parameter (thunk* #f)))

(define-values (do-routing url)
  (dispatch-rules
    [("top")
     (start
      (lambda (r)
        (welcome)))]
    [("run")
     (start
      (lambda (r)
        (define prog (id->thing (arg 'id)))

        (define v (arg 'value))
        (when v (redescribe! prog 'value v))

        (run-program prog)

        (with-embeds
            (response/json/cors
             (hash)))

        ))]

    ;;NONONO.  Should not be in here.
    [("twitch-spell")
     (lambda (r)
       
       ((on-twitch-spell) r)
       (with-embeds
           (response/json/cors
            (hash))))]

    [("eval-spell") ;To match the REST interface for codespells-server/main
     #:method "post"
     (lambda (r)
       (eval-spell r)

       (with-embeds
           (response/json/cors
            (hash)))
       )]

    ))

(define (serve-model g)
  (set! the-game g)
  (serve/servlet (lambda(r)
                   (displayln r)
                   (do-routing r))
                 #:port 8081
                 #:servlet-regexp #rx""
                 #:launch-browser? #f
                 #:stateless? #t))

