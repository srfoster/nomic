#lang at-exp racket

(provide start-game!
         end-turn!
         nexus
         sevarog
         parasite
         the-game
         add-watcher!
         take-action
         target-permanent
         (all-from-out nomic/base))

(require codespells
         taggable
         nomic/base
         nomic/util)

(define the-game
  (new-game))

(define/contract (set-the-game! g)
  (-> game? void?)
  (set! the-game g))

(define (start-game! . player-names)
  (set-the-game! (apply start-game the-game player-names)))

(define (end-turn!)
  (define current-index (index-of (game-players the-game)
                                  (game-current-player the-game)))
  (change-player! (list-ref (game-players the-game)
                           (modulo (+ current-index 1)
                                   (length (game-players the-game)))))
  (for ([i (map watcher-id (game-watchers the-game))])
    (add-mana-to-watcher! i 10))
  (call-begin-turn-callbacks the-game))

(define (add-mana-to-mana-construct! id mana)
  (set-the-game! (add-mana-to-mana-construct the-game id mana)))

(define (update-mana-construct! id new-mc)
  (set-the-game! (update-mana-construct the-game id new-mc)))

;Current turn??
;  When can players spawn stuff?
;  When can watchers do things?
;Make nexus/sevarog/etc into actions
(define (take-action a)
 (match-define (action type owner data turn) a)

  ;Check to see if owner can take this particular action at this particular time...
  (match type
    ['add-watcher! (add-watcher! data)]
    ['add-mana-to-watcher! (once-per-turn the-game a (thunk (add-mana-to-watcher! owner data)))]
    ['end-turn! (displayln "ENDING TURN")])

  ;We might not want to add action to action log if the action didn't succeed...
  (set-the-game! (struct-copy game the-game
                              [action-log
                               (cons (struct-copy action a
                                                  [turn (game-current-turn the-game)])
                                     (game-action-log the-game))]))
  )

(define (change-mana id m)
  (thunk
    (add-mana-to-mana-construct! id m)))

(define (change-player! p)
  (set-the-game! (change-player the-game p)))

(define (add-watcher! new-watcher)
  (set-the-game! (add-watcher the-game new-watcher)))

(define (update-watcher! id new-watcher)
  (set-the-game! (update-watcher the-game id new-watcher)))

(define (add-mana-to-watcher! watcher-id mana)
  (set-the-game! (add-mana-to-watcher the-game watcher-id mana)))


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
  
  (define (crystals color)
    (local-require (except-in crystals my-mod-lang))
    (match color
      ['pink   dark-crystal]
      ['green  green-crystal]
      ['orange orange-crystal]
      ['blue   blue-crystal]))
  
  (define thing-id (~a (player-name (game-current-player the-game)) ":Nexus:" (next-thing-id)))

  (set-the-game! (struct-copy game the-game
                              [mana-constructs
                               (cons
                                (mana-construct thing-id "Nexus" mana (player-name (game-current-player the-game))
                                                (list (on-beginning-of-turn (change-mana thing-id 25))))
                                (game-mana-constructs the-game))]))
  
  (with-tag thing-id
    (parentify
      (crystals crystal-color)
      (scale
       (xyz-vector 3 3 3)
       basic-circle)))

  )

(define (has-enough-mana? ms m)
  (>= (mana-construct-mana ms) m))

(define (remove-mana! ms m)
  (set-the-game! (struct-copy game the-game
                              [mana-constructs
                               (map (lambda (mc)
                                      (define current-mana (mana-construct-mana ms))
                                      (if (eq? mc ms)
                                          (struct-copy mana-construct mc [mana (- current-mana m)])
                                          mc))
                                    (game-mana-constructs the-game))])))

(define (current-player-nexus)
  (findf (lambda (mc)
           (and (string=? (mana-construct-owner mc) (player-name (game-current-player the-game)))
                (string=? (mana-construct-name mc) "Nexus")))
         (game-mana-constructs the-game)))

(define (sevarog #:mana mana)
  (local-require (except-in sevarog my-mod-lang)
                 (except-in hierarchy my-mod-lang))

  (only-if-enough-mana mana
                       (thunk
                        (define thing-id (~a (player-name
                                              (game-current-player the-game))
                                             ":Sevarog:"
                                             (next-thing-id)))
                        (set-the-game! (struct-copy game the-game
                                                    [mana-constructs
                                                     (cons
                                                      (mana-construct thing-id
                                                                      "Sevarog"
                                                                      mana
                                                                      (player-name (game-current-player the-game))
                                                                      (list))
                                                      (game-mana-constructs the-game))]))
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
    (id->mana-construct the-game (mana-channel-id mc)))
  
  (if (has-enough-mana? mana-source mana)
      (let ()
        (remove-mana! mana-source mana)
  
        (function))
      (let ()
        (ice-ball-hit))))

(define (do-all . fs)
  (thunk (map (lambda (f)
                (f))
              fs)))

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
  (define mc (id->mana-construct the-game mc-id))
  (define copy (struct-copy mana-construct mc))
  (displayln "######COPY#######"))

(define (parasite #:mana mana #:regen [regen 33] #:behavior [behavior (thunk (displayln "HOWDY"))])
  (local-require (except-in hierarchy my-mod-lang)
                 (except-in fire-particles my-mod-lang))

  (define thing-id (~a (player-name (game-current-player the-game))
                       ":Parasite:"
                       (next-thing-id)))

  (only-if-enough-mana mana
                       (thunk
                        (set-the-game! (struct-copy game the-game
                                                    [mana-constructs
                                                     (cons
                                                      (mana-construct thing-id
                                                                      "Parasite"
                                                                      mana
                                                                      (player-name (game-current-player the-game))
                                                                      (list (on-beginning-of-turn
                                                                             (do-all
                                                                              (change-mana thing-id regen)
                                                                              (change-mana-of-target thing-id (- regen))
                                                                              behavior))))
                                                      (game-mana-constructs the-game))]))

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