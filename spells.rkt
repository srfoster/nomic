#lang at-exp codespells

(require nomic/lang
         taggable)

(provide nexus
         sevarog
         parasite)

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
       basic-circle))))

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