#lang at-exp racket

(provide start-game!
         end-turn!
         set-the-game!
         the-game
         do-all
         add-watcher!
         change-mana
         change-mana-of-target
         only-if-enough-mana
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

