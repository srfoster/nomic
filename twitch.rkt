#lang racket/base

(provide #%module-begin
         #%app
         #%datum
         #%top
         #%top-interaction

         cheer
         middle
         high-middle
         red
         blue
         watch
         with-twitch-id
         watcher?
         watcher-id
         meditate

         let

         ;Be careful what you provide!
         ; For example: Not current-twitch-id, or else users can impersonate each other
         ; Also nothing with side effects.
         ;
         ;Good rule of thumb: Only provide functions that return values.
         ;  e.g. (watch) returns a (watcher (current-twitch-id) 10).
         ;  Adding this to all-watchers is the job of something outside the eval context.
         ;  So don't do it here.
         )

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(require (only-in fire-particles explosion)
         (only-in ice-particles ice-ball-hit)
         (only-in codespells at)
         nomic/lang
         )

(struct xyz (x y z))

(define middle
  (xyz -4553.495605 -6382.978516 15681.584961))

(define high-middle
  (xyz -4553.495605 -5082.978516 15681.584961))

(define red
  explosion)

(define blue
  ice-ball-hit)

(define (cheer #:color [color explosion] #:at [place middle])
  (at [(xyz-x place) (xyz-y place) (xyz-z place)]
      (color)))

(define current-twitch-id (make-parameter #f))
(define-syntax-rule (with-twitch-id twitch-id lines ...)
  (if (current-twitch-id)
      (error "Cannot redefine twitch id!")
      (parameterize ([current-twitch-id twitch-id])
          (let ()
            lines ...
            ))))

(define (watch)
  (action
   'add-watcher!
   (current-twitch-id)
   (watcher (current-twitch-id)
            10)))

(define (meditate)
  (action
   'add-mana-to-watcher!
   (current-twitch-id)
   10))