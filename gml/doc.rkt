#lang racket

;Random Ideas...
;Origins of the multiverse.
;A tale of 2 (3?) universes...
;  * Gods, wizards, cyberpunk
;"Zero Sum"
;NOTE: "Non-linear game editor" / Bottom up game design/definition...



;Begin...

(require (submod nomic/gml/base games//relations))

;Wanna play a game?

(define you
  (thing #:name "You"))

(define me
  (thing #:name "Me"))

(define the-game
  (put-in (new-game)
          you
          me))

;Maybe we could each have things...

(define your-things
  (thing #:name "Your Things"
         #:value '()))

(define my-things
  (thing #:name "My Things"
         #:value '()))

(define the-game-with-things
  (put-in (new-game)
          you
          me
          your-things
          my-things))

;But shouldn't we have some rules?

(define rules
  (thing #:name "Rules"
         #:value '()))

(define the-game-with-things-and-rules
  (put-in (new-game)
          rules
          you
          me
          your-things
          my-things))

;TODO: Make react renderer for games!


;How do we describe rules?  A list, perhaps?

(redescribe! rules
             'value
             '(1))


;Let's make an example rule.  Maybe...

(define concept-of-turn
  (thing #:name "Concept of Turn"
         #:value "My Turn"
         'on-turn-change
         '(redescribe! (my-self) 'value (if (string=? "My Turn"
                                                      (value #:of (my-self)))
                                            "Your Turn"
                                            "My Turn"))))

;Let's agree on what concept of turn means

(require (submod nomic/gml/base VM)
         rackunit)


(check-equal? "My Turn"
              (value #:of concept-of-turn))

(run 'on-turn-change #:of concept-of-turn)

(check-equal? "Your Turn"
              (value #:of concept-of-turn))

(set! the-game-with-things-and-rules
      (put-in (new-game)
              rules
              concept-of-turn
              you
              me
              your-things
              my-things))

;(describe-things-in the-game-with-things-and-rules)

;But should turns concept be a rule?

(redescribe! rules
             'value (list concept-of-turn))

(set! the-game-with-things-and-rules
      (put-in (new-game)
              rules
              you
              me
              your-things
              my-things))

(describe-things-in the-game-with-things-and-rules)

;Can always see rules whenever we want
(map describe-thing (value #:of rules))


;Now, let's add another rule.

(define concept-of-summoning ;How to make minimal?
  (thing #:name "Concept Of Summoning"
         #:value (thing #:name "Thing to be Summoned")))

;Design branch:
;A) Can you only summon on your turn?
;B) Can summoning happen any time?

;Lets do A first:

(define (do-summon t)
  (define the-things
    (if (string=?
       "My Turn"
       (value #:of concept-of-turn))
      my-things your-things))

  (redescribe! the-things
               'value
               (cons t (value #:of the-things))))


(redescribe! rules
             'value (list concept-of-turn
                          concept-of-summoning))

(redescribe! concept-of-turn
             'value "My Turn")

(check-equal? 0
              (length (value #:of my-things)))

(check-equal? 0
              (length (value #:of your-things)))

(define my-thing (thing #:name "My Thing"))
(do-summon my-thing)

(run 'on-turn-change #:of concept-of-turn)

(define your-thing (thing #:name "Your Thing"))
(do-summon your-thing)

(describe-things-in the-game-with-things-and-rules)

(check-equal? 1
              (length (value #:of my-things)))

(check-equal? 1
              (length (value #:of your-things)))


;Design branch / loop
;Broad: Relations.  Things can be [on/in/attatched-to] each other...
;Specific: Things can have mana?


;Broad

(define your-thing-2 (thing #:name "Your Thing 2"))
(do-summon your-thing-2) ;(Perhaps you are winning now)

(check-equal? #f
              (related? your-thing 'on your-thing-2))

(do-summon
 (relation your-thing 'on your-thing-2))

(check-not-false #t
                 (related? your-thing 'on your-thing-2))

(map describe-thing (value #:of your-things))


;2nd Iter: Design branch / loop
;Ideate.  What is fun? (What achieves our values?)
;  What's the most general/abstract thing we can do?
;Do it/(them in parallel?).  Loop again.

;Ideation:
;Things can [verb:attack/kill/help] each other
;Things can have [data:mana/health/speed] etc?


(run 'on-turn-change #:of concept-of-turn)

(define mana-for-my-thing
  (thing #:name "Mana"
         #:type 'Mana
         #:value 10))
(do-summon mana-for-my-thing)
(do-summon (relation my-thing 'has mana-for-my-thing))


(map describe-thing
 (find-relations my-thing
                 'has))

(define (is-mana-relation? mr)
  (and
   (eq? (type #:of mr) 'Relation)
   (eq? (type #:of (relation-to mr)) 'Mana)))

(define (mana #:of t)
  (apply +
         (map (compose (lambda (r) (value #:of r)) relation-to)
              (filter
               is-mana-relation?
               (find-relations t 'has)))))


(check-equal? (mana #:of my-thing)
              10)



;3rd Iter: Design branch / loop
;Ideate.  What is fun? (What achieves our values?)
;  What's the most general/abstract thing we can do?
;  What's fun to write/explore?
;Do it/(them in parallel?).  Loop again.

;Ideation:
;Things can [verb:attack/kill/help] each other
;Things can have [data:mana/health/speed] etc?
;Geometric relationships.  (Renderer...)
;Things can die... Summon / Unsummon.


