#lang at-exp racket

(provide start-game
         end-turn!
         action-log
         nexus
         sevarog
         parasite
         current-player
         change-player
         all-players
         all-mana-constructs
         all-watchers
         add-watcher!
         (struct-out mana-construct)
         (struct-out mana-channel)
         (struct-out watcher)
         (struct-out player)
         (except-out (struct-out action) action)
         (rename-out [make-action action])
         take-action)

(require codespells
         "./base.rkt")

(struct mana-construct (id name mana owner callbacks) #:prefab)
(struct callback (type function) #:prefab)
(struct watcher (id mana) #:prefab)
(struct mana-channel (id mana) #:prefab)
(struct action (type owner data turn) #:prefab)
(struct player (name) #:prefab)

;Consolidate into a game state struct??

(define current-player #f)
(define current-turn 0)
(define all-mana-constructs '())
(define all-watchers '())
(define action-log '())
(define all-players '())

(define (on-beginning-of-turn f)
  (callback 'begin-turn f))

(define (change-mana id m)
  (thunk
    (displayln (list "Changing Mana..." id m))
    (add-mana-to-mana-construct! id m)))

(define (add-mana-to-mana-construct! id mana)
  (define mc (id->mana-construct id))
  (update-mana-construct! id
                          (struct-copy mana-construct
                                       mc [mana
                                           (+ (mana-construct-mana mc) mana)])))

(define (update-mana-construct! id new-mc)
  (set! all-mana-constructs
        (map (lambda (mc)
               (if (string=? id (mana-construct-id mc))
                   new-mc
                   mc))
             all-mana-constructs)))

(define (start-game . player-names)
  (set! all-players (map (lambda (n)
                           (player n))
                         player-names))
  (set! current-player (first all-players)))

(define (make-action type owner data)
  (action type owner data #f))

(define (end-turn!)
  (define current-index (index-of all-players current-player))
  (change-player (list-ref all-players
                           (modulo (+ current-index 1) (length all-players))))
  (for ([i (map watcher-id all-watchers)])
    (add-mana-to-watcher! i 10))
  (call-begin-turn-callbacks))

(define (call-begin-turn-callbacks)
  (define current-player-mcs
    (filter belongs-to-current-player?
            all-mana-constructs))
  (define begin-turn-cbs
    (filter is-begin-turn-callback?
            (flatten (map mana-construct-callbacks current-player-mcs))))
  (map call-callback begin-turn-cbs))

(define (call-callback cb)
  ((callback-function cb)))

(define (is-begin-turn-callback? cb)
  (eq? 'begin-turn (callback-type cb)))

(define (belongs-to-current-player? mc)
  (string=? (mana-construct-owner mc) (player-name current-player)))

;Current turn??
;  When can players spawn stuff?
;  When can watchers do things?
;Make nexus/sevarog/etc into actions

(define (take-action a)
 (match-define (action type owner data turn) a)

  ;Check to see if owner can take this particular action at this particular time...

  (match type
    ['add-watcher! (add-watcher! data)]
    ['add-mana-to-watcher! (once-per-turn a (thunk (add-mana-to-watcher! owner data)))]
    ['end-turn! (displayln "ENDING TURN")]
    )

  ;We might not want to add action to action log if the action didn't succeed...
  (set! action-log (cons (struct-copy action a [turn current-turn]) action-log)))

(define (once-per-turn a f)
  (when (not (has-already-run-this-turn? a))
    (f)))

(define (has-already-run-this-turn? new-a)
  (findf (lambda (a)
           (and (string=? (action-owner a) (action-owner new-a))
                (eq? (action-type a) (action-type new-a))
                (= (action-turn a) current-turn)))
         action-log))

(define (change-player p)
  (set! current-player p)
  (set! current-turn (+ current-turn 1)))

(define current-thing-id -1)

(define (next-thing-id)
  (set! current-thing-id (add1 current-thing-id))
  current-thing-id)

(define (add-watcher! new-watcher)
  (when (not (find-watcher (watcher-id new-watcher)))
    (set! all-watchers
          (cons new-watcher
                all-watchers))))

(define (update-watcher! id new-watcher)
  (set! all-watchers
        (map (lambda (w)
               (if (string=? id (watcher-id w))
                   new-watcher
                   w))
             all-watchers)))

(define (add-mana-to-watcher! watcher-id mana)
  (define w (find-watcher watcher-id))
  (update-watcher! watcher-id
                   (struct-copy watcher
                                w [mana
                                   (+ (watcher-mana w) mana)])))

(define (find-watcher id)
  (findf (lambda (w)
           (string=? id (watcher-id w)))
         all-watchers))

(define (crystals color)
  (local-require (except-in crystals my-mod-lang))
  (match color
    ['pink   dark-crystal]
    ['green  green-crystal]
    ['orange orange-crystal]
    ['blue   blue-crystal]))

;;Possible additional parameters
  ; Mana cap / max mana
  ; Regen rate
  ; Starting mana
  ; Radius
(define (nexus #:mana mana
               #:color [crystal-color 'pink])
  (local-require (except-in hierarchy my-mod-lang)
                 (except-in crystals my-mod-lang)
                 (except-in magic-circles my-mod-lang))
  
  (define thing-id (~a (player-name current-player) ":Nexus:" (next-thing-id)))

  (set! all-mana-constructs
        (cons
         (mana-construct thing-id "Nexus" mana (player-name current-player)
                         (list (on-beginning-of-turn (change-mana thing-id 25))))
         all-mana-constructs))
  
  (with-tag thing-id
    (parentify
      (crystals crystal-color)
      (scale
       (xyz-vector 3 3 3)
       basic-circle)))

  ;thing-id
  )

(define (id->mana-construct id)
  (define ret
    (findf
     (lambda (mc)
       (string=? id (mana-construct-id mc)))
     all-mana-constructs))
  (when (not ret)
    (error "Mana construct with ID not found: " id))
  ret)

(define (has-enough-mana? ms m)
  (>= (mana-construct-mana ms) m))

(define (remove-mana! ms m)
  (set! all-mana-constructs
        (map (lambda (mc)
               (define current-mana (mana-construct-mana ms))
               (if (eq? mc ms)
                   (struct-copy mana-construct mc [mana (- current-mana m)])
                   mc))
             all-mana-constructs)))

(define (current-player-nexus)
  (findf (lambda (mc)
           (and (string=? (mana-construct-owner mc) (player-name current-player))
                (string=? (mana-construct-name mc) "Nexus")))
         all-mana-constructs))

(define (sevarog #:mana mana)
  (local-require (except-in sevarog my-mod-lang)
                 (except-in hierarchy my-mod-lang))

  (only-if-enough-mana mana
                       (thunk
                        (define thing-id (~a (player-name current-player) ":Sevarog:" (next-thing-id)))
                        (set! all-mana-constructs
                              (cons
                               (mana-construct thing-id "Sevarog" mana (player-name current-player) (list))
                               all-mana-constructs))
                        ; L/(1+e^-k*(x-x0))
                        ; k is steepness, L is maximum value and x0 is size at 0
                        ; THANKS KENZO! :)
                        (define n
                          (* 0.02 mana))
  
                        (with-tag thing-id
                          (scale (xyz-vector n n n) (sevarog))))))

(define (only-if-enough-mana mana function)
  (local-require (except-in ice-particles my-mod-lang))
  (define mc
    (mana-channel (mana-construct-id (current-player-nexus)) m))

  (define mana-source
    (id->mana-construct (mana-channel-id mc)))
  
  (if (has-enough-mana? mana-source mana)
      (let ()
        (remove-mana! mana-source mana)
  
        (function))
      (let ()
        (ice-ball-hit))))

(define (both a b)
  (thunk (a)
         (b)))

(define (target-of id)
  (lambda ()
    (unreal-eval-js @~a{
 (function(){
  let HttpHelper = Root.ResolveClass("HttpHelper");
  let helper = new HttpHelper(GWorld);
  helper.Post("http://localhost:@(codespells-server-port)/messages",
  JSON.stringify({message:{targetId: "THIS IS THE ID"}}))
  })()})
    "THIS IS THE ID"))

(define (change-mana-of-target id mana)
  (local-require codespells-server/ui/user-generated)
  
  (lambda ()
    (unreal-eval-js @~a{
 (function(){
  let HttpHelper = Root.ResolveClass("HttpHelper");
  let helper = new HttpHelper(GWorld);
  let current = global.taggedThings["@id"];
  let target = current.GetAttachParentActor();
  let target_id = undefined;
  for(var tag in global.taggedThings){
    if(target == global.taggedThings[tag]){
      target_id = tag;
   }
  }
  helper.Post("http://localhost:@(codespells-server-port)/messages",
  JSON.stringify({message:{targetId: target_id}}))
  })()})
    (sleep 1)
    (define target-id (hash-ref (hash-ref (get-last-unreal-message) 'message) 'targetId))
    ((change-mana target-id mana))))

(define (spawn-copy mc-id #:mana mana)
  (define mc (id->mana-construct mc-id))
  (define copy (struct-copy mana-construct mc))
  (displayln "######COPY#######"))

(define (parasite #:mana mana #:regen regen #:behavior [behavior (thunk (displayln "HOWDY"))])
  (local-require (except-in hierarchy my-mod-lang)
                 (except-in fire-particles my-mod-lang))

  (define thing-id (~a (player-name current-player)
                       ":Parasite:"
                       (next-thing-id)))

  (only-if-enough-mana mana
                       (thunk
                        (set! all-mana-constructs
                              (cons
                               (mana-construct thing-id "Parasite" mana (player-name current-player)
                                               (list (on-beginning-of-turn
                                                      (both
                                                       (change-mana thing-id regen)
                                                       (change-mana-of-target thing-id (- regen))
                                                       behavior))))
                               all-mana-constructs))

                        (with-tag thing-id
                           @unreal-js{
 (function(){
  var child = @(at [0 0 0] (flames))
  var parent = @(parentify
                 ;(find-with-tag "Lindsey:Sevarog:1")
                 (target-permanent)
                 @unreal-js{child})

  return child
  })()}
                           ))))