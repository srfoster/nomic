#lang racket

(require (only-in (submod nomic/gml/base games//relations) thing))

(provide #%module-begin
         #%app
         #%datum
         #%top
         #%top-interaction
         quote

         :
         watch
         with-twitch-id)


(define (watch)
  (hash 'type 'Watcher
        'name (current-twitch-id)))

(define (: k v)
  (when (or (eq? k 'name) (eq? k 'type))
    (error "You can't change your name or type~!"))
  (hash 'type 'WatcherDescribe
        'key     k
        'value   v))

(define current-twitch-id (make-parameter #f))
(define-syntax-rule (with-twitch-id twitch-id lines ...)
  (if (current-twitch-id)
      (error "Cannot redefine twitch id!")
      (parameterize ([current-twitch-id twitch-id])
          (let ()
            lines ...
            ))))
