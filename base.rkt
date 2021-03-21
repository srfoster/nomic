#lang at-exp racket

(require codespells)

(provide (struct-out game)
         (struct-out mana-construct)
         (struct-out mana-channel)
         (struct-out callback)
         (struct-out watcher)
         (struct-out player)
         (struct-out action)
         make-action
         new-game
         update-mana-construct
         add-mana-to-mana-construct
         id->mana-construct
         start-game
         on-beginning-of-turn
         call-begin-turn-callbacks
         once-per-turn
         change-player
         current-thing-id
         next-thing-id
         add-watcher
         update-watcher
         add-mana-to-watcher
         )

(struct mana-construct (id name mana owner callbacks) #:prefab)
(struct mana-channel (id mana) #:prefab)
(struct callback (type function) #:prefab)
(struct watcher (id mana) #:prefab)
(struct player (name) #:prefab)
(struct action (type owner data turn) #:prefab)

(define (make-action type owner data)
  (action type owner data #f))

(struct game (players current-player current-turn action-log mana-constructs watchers))

(define (new-game)
  (game '() #f 0 '() '() '()))

(define current-thing-id -1)

(define (next-thing-id)
  (set! current-thing-id (add1 current-thing-id))
  current-thing-id)

(define (start-game g . player-names)
  (displayln player-names)
  (struct-copy game g
               [players
                (map (lambda (n)
                       (player n))
                     player-names)]
               [current-player
                (player (first player-names))]))

(define (update-mana-construct g id new-mc)
  (struct-copy game g
               [mana-constructs
                (map (lambda (mc)
                       (if (string=? id (mana-construct-id mc))
                           new-mc
                           mc))
                     (game-mana-constructs g))]))

(define (add-mana-to-mana-construct g id mana)
  (define mc (id->mana-construct g id))
  (update-mana-construct g id
   (struct-copy mana-construct
               mc [mana
                   (+ (mana-construct-mana mc) mana)])))

(define (change-player g p)
  (define new-g (struct-copy game g
                             [current-player p]))
  (define new-g2 (struct-copy game new-g
                              [current-turn (+ (game-current-turn new-g) 1)]))
  new-g2)

(define (id->mana-construct g id)
  (define ret
    (findf
     (lambda (mc)
       (string=? id (mana-construct-id mc)))
     (game-mana-constructs g)))
  (when (not ret)
    (error "Mana construct with ID not found: " id))
  ret)

(define (on-beginning-of-turn f)
  (callback 'begin-turn f))

(define (call-begin-turn-callbacks g)
  (define current-player-mcs
    (filter (curry belongs-to-current-player? g) 
            (game-mana-constructs g)))
  (define begin-turn-cbs
    (filter is-begin-turn-callback?
            (flatten (map mana-construct-callbacks current-player-mcs))))
  (map call-callback begin-turn-cbs))

(define (call-callback cb)
  ((callback-function cb)))

(define (is-begin-turn-callback? cb)
  (eq? 'begin-turn (callback-type cb)))

(define (belongs-to-current-player? g mc)
  (string=? (mana-construct-owner mc) (player-name (game-current-player g))))

(define (once-per-turn g a f)
  (when (not (has-already-run-this-turn? g a))
    (f)))

(define (has-already-run-this-turn? g new-a)
  (findf (lambda (a)
           (and (string=? (action-owner a) (action-owner new-a))
                (eq? (action-type a) (action-type new-a))
                (= (action-turn a) (game-current-turn g))))
         (game-action-log g)))

(define (add-watcher g new-watcher)
  (if (not (find-watcher g (watcher-id new-watcher)))
    (struct-copy game g
                 [watchers
                  (cons new-watcher
                        (game-watchers g))])
    g))

(define (find-watcher g id)
  (findf (lambda (w)
           (string=? id (watcher-id w)))
         (game-watchers g)))

(define (update-watcher g id new-watcher)
  (struct-copy game g
               [watchers
                (map (lambda (w)
                       (if (string=? id (watcher-id w))
                           new-watcher
                           w))
                     (game-watchers g))]))

(define (add-mana-to-watcher g watcher-id mana)
  (define w (find-watcher g watcher-id))
  (update-watcher g watcher-id
                  (struct-copy watcher
                               w [mana
                                  (+ (watcher-mana w) mana)])))