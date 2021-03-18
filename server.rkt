#lang at-exp racket

(require "./lang.rkt"
         web-server/servlet
         web-server/servlet-env
         (except-in website-js header)
         codespells-server/ui/util)

(provide start-game
         (all-from-out "./lang.rkt"))

(define (render-game-state)
  (card
   (card-body
     (ul
      (map li (map render-mana-construct all-mana-constructs))))))

(define (render-mana-construct maco)
  (~a "ID: " (mana-construct-id maco) " | Mana: " (mana-construct-mana maco) " | Owner: " (mana-construct-owner maco)))

(define (show-state r)
  (response/html/content 
   (enclose
    (container
     (p "GAME STATE:")
     (render-game-state)
     (p "ACTION LOG:")
     (ul (map li action-log)))
    (script ([dummy @js{
               setInterval(()=>{window.location.reload()},2000);
               }])))))

(define-values (routing url)
  (dispatch-rules
   [("game")
    show-state]))

(define (start-game)
  (serve/servlet routing
                 #:port 9000
                 #:servlet-regexp #rx""
                 #:servlet-path "/game"
                 #:launch-browser? #t
                 #:extra-files-paths
                 (list website-bootstrap-path)
                 #:servlet-current-directory (current-directory)))

