#lang racket

(require nomic/game/lib)

(define (nexus mana)
  (mana-construct
   #:looks-like (blueprint crystal-with-circle)

   (with-data 'rate 25)

   (on-beginning-of-turn
    (gain-mana (data 'rate)))
   
   ))

(define (wraith mana)
  (mana-construct
   #:looks-like (blueprint sevarog
                           #:scaled-by mana
                           #:max 2) 
   ))

(define (parasite mana)
  (mana-construct
   #:looks-like (blueprint flames)

   (data 'rate 25
         'victim (targeted-creature))

   (on-summon
    (attatch-to (data 'victim)))

   (on-beginning-of-turn
    (subtract-mana (data 'rate)
                   #:from (data 'victim))
    (gain-mana (data 'rate))
    
    ;example... Can we pull this out?
    ;  How are complex behaviors like this stored?  As programs?
    (when (>= (current-mana) 50)
      (spawn-copy #:owner (current-player)
       (data 'victim))))))







