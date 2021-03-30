#lang racket/base

#|
  Goals:

  Make a game description language

  * That can describe all/most human games
  * Whose descriptions can turn into real games (i.e. via CodeSpells)
  * Can express meta games like nomic.  Rules are part of game state.
  * Maybe can run in the Ethereum VM
  * Can automatically make "bots" -- players of any game that play randomly
  * Can be used in the machine learning loop
  * Can be used to automatically generate new games
  * Lets people fork games and alter their rules, extend them, etc.
  * Matches the human language we use to define game rules
  * Is built on top of extremely simple primitives (a simple vm)
  * Can put real humans in loop with continuations, web requests, etc.
  * Has renderers that automate gamestate -> web, -> unreal, etc for
    whole game types.  Easy to extend renderers when extending games
  * Makes the game Lindsey and I are developing easier to develop!

|#

(module game/things racket/base
  (provide (except-out (struct-out thing) thing)
           
           game?
           take-out
           put-in
           new-game
           things-in
           id->thing
           (rename-out [get-all-things all-things]
                       [new-thing thing]))

  (require racket/contract)

  (define next-id 0)
  (struct thing (id))
  (struct game (things) #:transparent)


  (define all-things '())
  (define (get-all-things) all-things)

  (define (id->thing i)
    ;Hmmm.  Should keep a table
    ;(displayln i)
    (findf (lambda (t)
              (= (thing-id t) i))
            all-things))
  
  (define/contract (new-thing)
    (-> thing?)
    (define ret (thing next-id))

    (set! next-id (add1 next-id))

    (set! all-things (cons ret all-things))
    
    ret)

  (define/contract (things-in g)
    (-> game? (listof thing?))
    (game-things g))


  (define/contract (new-game)
    (-> game?)
    (game '()))

  (define (put-in g . t)
    (-> game? #:rest thing? game?)
    (struct-copy game g
                 [things (append t
                                 (game-things g))]))

  (define (take-out g t)
    (-> game? thing? game?)
    (struct-copy game g
                 [things (filter (lambda (t0) (eq? t t0))
                                 (game-things g))]))

  (module+ test
    (require rackunit)
    
    (define g (new-game))
    (define left-foot (new-thing))
    (define left-foot-in (put-in g left-foot))
    (define left-foot-out (take-out g left-foot))

    (check-equal?   
     (length (things-in left-foot-in))
     1)

    (check-equal?   
     (length (things-in left-foot-out))
     0)
    ))

(require (submod "." game/things test))

(module game/things/descriptions racket/base
  (provide thing what-is
           thing-named
           print-game
           describe-things-in
           redescribe!
           describe-thing
           thingify
           parent-of
           (except-out
            (all-from-out (submod ".." game/things))
            lower:thing))
  
  (require
    (except-in (submod ".." game/things) thing)
    (rename-in (submod ".." game/things)
               [thing lower:thing])
    racket/hash )

  (define descriptions (hash))
  (define parents (hash))
  
  (define (parent-of t)
    (hash-ref parents t))

  (define (thingify td)
    (define t (lower:thing))
    (set! descriptions (hash-set descriptions
                                 t
                                 (hash)))
    (for ([k (hash-keys td)])
      (redescribe! t k (hash-ref td k)))
    t)
  
  (define (thing . attrs/vals)
    (define ret (lower:thing))

    (for ([i (length attrs/vals)])
      (when (odd? i)
        (define child (list-ref attrs/vals i))

        (when (thing? child)
          (set! parents (hash-set parents
                                  child
                                  ret)))))
    
    (set! descriptions (hash-set descriptions
                                 ret
                                 (apply hash attrs/vals)))
    ret)

  (define (redescribe! t k v)
    (when (thing? v)
          (set! parents (hash-set parents
                                  k
                                  t)))


    ;The tests show this to be correct, but it is getting hard to follow.
    ;  Refactor to show correctness
    (set! descriptions
          (hash-set descriptions
                    t
                    (if (procedure? v)
                        (hash-set (hash-ref descriptions t)
                                  k (v (hash-ref
                                        (hash-ref descriptions t)
                                        k)))
                        (hash-set (hash-ref descriptions t)
                                  k v))

                    ))

    )

  (define (what-is k #:for [for #f]
                   #:of [of #f]
                   #:fallback [fallback (void)])
    (define the-thing (or of for))
    (when (not the-thing) (raise "Must supply #:for or #:of to (what-is ...)"))
    (hash-ref (hash-ref descriptions the-thing) k fallback))

  
  (define (thing-named s #:in g)
    (findf (lambda (t)
             (string=? s
                       (what-is 'name #:of t)))
           (things-in g)))

  (define (print-game g)
    (for ([t (things-in g)])
      (displayln (hash-ref descriptions t))))

  (define (describe-things-in g)
    (for/list ([t (things-in g)])
      (hash-ref descriptions t)))

  (define (describe-thing t)
    (hash-set (hash-ref descriptions t)
              'id
              (thing-id t)))

  (module+ test
    (require rackunit racket/list)

    (define g0 (new-game))

    (define board (thing 'name "Board"))
    (define queen (thing 'name "Light Queen"
                         'on-square: "d1" ))

    (define g1 (put-in g0 queen))

    (check-equal?
     (what-is 'name #:of (first (things-in g1)))
     "Light Queen")

    ;Since things can only be described by primitive values (not functions),
    ;  we can pass in a function and it can be used to update the current value
    
    (redescribe! queen 'name string-downcase)

    (check-equal?
     (what-is 'name #:of (first (things-in g1)))
     "light queen")
    
    ))

(require (submod "." game/things/descriptions test))


(module game/things/descriptions/names racket/base
  (provide thing name
           (except-out
            (all-from-out (submod ".." game/things/descriptions))
            lower:thing))
  
  (require
    (except-in (submod ".." game/things/descriptions) thing)
    (rename-in (submod ".." game/things/descriptions)
               [thing lower:thing]))

  (define (thing #:name n . attrs/vals)
    (apply lower:thing (append (list 'name n) attrs/vals)))

  (define (name #:of t)
    (what-is 'name #:of t))

  (module+ test
    (require rackunit racket/list)
    
    (define board (thing #:name "Board"))

    (check-equal? (name #:of board)
                  "Board")))

(require (submod "." game/things/descriptions/names test))

(module game/things/descriptions/names/types racket/base
  (provide thing type
           (except-out
            (all-from-out (submod ".." game/things/descriptions/names))
            lower:thing))
  
  (require
    (except-in (submod ".." game/things/descriptions/names) thing)
    (rename-in (submod ".." game/things/descriptions/names)
               [thing lower:thing]))

  (define (thing
           #:name n
           #:type [t 'Thing] . attrs/vals)
    (when (not (symbol? t))
      (raise "#:type must be a symbol"))
    (apply lower:thing #:name n
           (append
            (list 'type t)
            attrs/vals)))

  (define (type #:of t)
    (what-is 'type #:of t))

  (module+ test
    (require rackunit racket/list)
    
    (define board (thing #:name "Board"
                         #:type 'Board))

    (check-equal? (type #:of board)
                  'Board)))

(require (submod "." game/things/descriptions/names/types test))




(module game/things/descriptions/names/types/values racket/base
  (provide thing value value-of
           (except-out
            (all-from-out (submod ".." game/things/descriptions/names/types))
            lower:thing))
  
  (require
    (except-in (submod ".." game/things/descriptions/names/types) thing)
    (rename-in (submod ".." game/things/descriptions/names/types)
               [thing lower:thing]))

  (define (thing
           #:name n
           #:type [t 'Thing]
           #:value [v (void)] . attrs/vals)

    (apply lower:thing #:name n #:type t
           (append
            (list 'value v)
            attrs/vals)))

  (define (value #:of t)
    (what-is 'value #:of t))

  (define (value-of t)
    (value #:of t))

  (module+ test
    (require rackunit racket/list)
    
    (define left-foot (thing #:name "Left Foot"
                             #:type 'Foot
                             #:value 'in))

    (check-equal? (value #:of left-foot)
                  'in)))

(require (submod "." game/things/descriptions/names/types/values test))









(module games//relations racket/base
  (provide relation related?
           find-relations
           relation-from relation-to relation-name
           (all-from-out (submod ".." game/things/descriptions/names/types/values)))
  
  (require
    (except-in (submod ".." game/things/descriptions/names/types/values) thing)
    (rename-in (submod ".." game/things/descriptions/names/types/values)
               [thing lower:thing])
    racket/contract)
  
  (require
    (submod ".." game/things/descriptions/names/types/values)
    racket/list)

  (define (relation from name to)
    (define ret
      (thing #:name "Relation"
             #:type 'Relation
             #:value (list from name to)))

    ret)

  (define/contract (related? from name to)
    (-> thing? symbol? thing? any/c)
    (findf
     (lambda (t)
       (equal? (value #:of t)
               (list from name to)))
     (all-things)))

  (define relation-to (compose third value-of))
  (define relation-from (compose first value-of))
  (define relation-name (compose second value-of))
  
  (define/contract (find-relations from name)
    (-> thing? symbol? (listof thing?))
    (filter
     (lambda (t)
       (and
        (eq? 'Relation (type #:of t))
        (equal? from (first (value #:of t)))
        (equal? name (second (value #:of t)))))
     (all-things)))

  (module+ test
    (require rackunit racket/list)

    (define g0 (new-game))

    (define board (thing #:name "Board"
                         #:type 'Board))
    (define d1    (thing #:name "d1"
                         #:type 'Piece)) 
    (define light-queen (thing #:name "Light Queen"
                               #:type 'Piece))

    (define d1:part-of:board     (relation d1
                                           'part-of
                                           board))
    (define light-queen-location (relation light-queen 'on d1))

    (define g (put-in g0
                      board
                      d1
                      light-queen
                      d1:part-of:board
                      light-queen-location))

    (check-not-false (related? d1 'part-of board))))

(require (submod "." games//relations test))



(module games//graphs racket/base
  (provide 
    new-thing-graph
    path
    link!

    grid-graph
    get-tile/xy

    (all-from-out (submod ".." game/things/descriptions/names/types/values)))
  
  (require
    (except-in (submod ".." game/things/descriptions/names/types/values) thing)
    (rename-in (submod ".." game/things/descriptions/names/types/values)
               [thing lower:thing])
    racket/contract
    racket/function
    graph)
  
  (require
    (submod ".." game/things/descriptions/names/types/values)
    racket/list)

  (define (new-thing-graph . vs)
    (define g (weighted-graph/directed '()))
    
    (map (curry add-vertex! g) vs)

    g)

  ;Tiles can be entered and exited.  But for all tiles in a graph,
  ;  a particular thing can only be on one at a time.

  (define (get-tile/xy g x y)
    ;Not very efficient.  Cache this value!
    (findf 
      (lambda (t)
	(and
	  (eq? x (what-is 'grid-x #:of t)) 
	  (eq? y (what-is 'grid-y #:of t)) )) 
      (get-vertices g)))

  (define (all-tiles g)
    (get-vertices g))

  (define (next-tile g source-t dest-t)
    (define the-path
      (path g source-t dest-t))

    (cond 
      [(not the-path) #f]
      [(< 1 (length the-path)) 
       (second the-path)]
      [(= 1 (length the-path)) 
       (first the-path)]))

  (define (exit-all-tiles g p)
    (for ([t (all-tiles g)])
	 (exit-tile p t)))

  (define (exit-tile p t)
    (define things (what-is 'things-on-tile #:of h #:fallback #f))
    (when (not things)
      (redescribe! t 'things-on-tile
		   '()))
    (redescribe! t 'things-on-tile
		 (curry remove p)))

  (define (enter-tile g p t)
    (exit-all-tiles g p)
    (redescribe! t 'things-on-tile
		 (curry cons p)))

  (define (current-tile g p)
    (findf
      (lambda (h)
	(member p (what-is 'things-on-tile #:of h #:fallback '())))
      (all-tiles g)))


  (define (move-toward-tile g p h)
    (define nh (next-tile g (current-tile g p) h))
    (enter-tile g p nh))


  (define (grid-graph 
	    #:constructor [constructor (curry thing #:name "Tile")]
	    w h)
    (define vs
      (for/list ([wi (range w)])
		(for/list ([hi (range h)])
			  (constructor 
			    'grid-x wi	
			    'grid-y hi))))

    (define g (apply new-thing-graph (flatten vs)))

    (for/list ([wi (range w)])
	      (for/list ([hi (range h)])
			(define current 
			  (list-ref (list-ref vs hi) wi))

			(define north
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (sub1 hi)) wi)))
			(define south
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (add1 hi)) wi)))
			(define east
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs hi) (add1 wi))))
			(define west
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs hi) (sub1 wi))))
			(define north-east
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (sub1 hi)) (add1 wi))))
			(define south-east
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (sub1 hi)) (add1 wi))))
			(define south-west
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (add1 hi)) (sub1 wi))))
			(define north-west
			  (with-handlers ([exn:fail? (thunk* #f)])
					 (list-ref (list-ref vs (sub1 hi)) (sub1 wi))))


			(and north
			     (add-edge! g current north))
			(and south
			     (add-edge! g current south))
			(and east
			     (add-edge! g current east))
			(and west
			     (add-edge! g current west))
			(and north-east
			     (add-edge! g current north-east))
			(and south-east
			     (add-edge! g current south-east))
			(and south-west
			     (add-edge! g current south-west))
			(and north-west
			     (add-edge! g current north-west))))

    g)

  (define (link! g v1 v2)
    (add-edge! g v1 v2))

  (define (path g v1 v2)
    (fewest-vertices-path g v1 v2))


  (module+ 
    test
    (require rackunit)

    ;TODO: Convenience functions for grids

    (define (house #:name n)
      (thing #:name n 'people '()))

    (define house1 (house #:name "House 1"))
    (define house2 (house #:name "House 2"))
    (define house3 (house #:name "House 3"))
    (define house4 (house #:name "House 4"))
    (define house5 (house #:name "House 5"))

    (define g (new-thing-graph house1 house2 house3 house4 house5))

    (link! g house1 house2)
    (link! g house1 house3)
    (link! g house1 house4)
    (link! g house4 house5)

    (check-eq? 4 (length (path g house2 house5))
	       "There should be 2 houses between house 2 and house 5: total 4 houses")

    (define dude (thing #:name "Dude"))
    (enter-tile g dude house2)

    (check-eq? 1 (length (what-is 'people #:of house2)))

    (move-toward-tile g dude house5)

    (check-eq? 0 (length (what-is 'people #:of house2)))
    (check-eq? 1 (length (what-is 'people #:of house1)))

    (move-toward-tile g dude house5)

    (check-eq? 0 (length (what-is 'people #:of house1)))
    (check-eq? 1 (length (what-is 'people #:of house4)))

    (move-toward-tile g dude house5)

    (check-eq? 0 (length (what-is 'people #:of house4)))
    (check-eq? 1 (length (what-is 'people #:of house5)))

    (move-toward-tile dude house5)
    (check-eq? 1 (length (what-is 'people #:of house5)))


    ;Test for grid graphs, e.g. chess

    (define new-g
      (grid-graph 5 5))

    (check-eq?
      5
      (length
	(path new-g
	      (get-tile/xy new-g 0 0)
	      (get-tile/xy new-g 3 4)
	      )))

    ))

(require (submod "." games//graphs test))


#;
(module chess racket/base
	(module+ test
		 (define square-names
		   (there-are-squares '((a3 b3 c3)
					(a2 b2 c2)
					(a1 b1 c1))))

		 (define square-values
		   (with-contents '((__ __ __)
				    (WP WP WP)
				    (WR WN WB))))
		 )
	)

#;
(module MtG racket/base

	(require
	  (submod ".." games//relations)
	  racket/contract)

	(module+ test
		 (define alice
		   (thing #:name "Alice"
			  #:type 'Player))

		 (define bob
		   (thing #:name "Bob"
			  #:type 'Player))



		 (define fireball (thing #:name "Fireball"
					 #:type 'Card))
		 (define goblin   (thing #:name "Goblin"
					 #:type 'Card))
		 (define mountain (thing #:name "Mountain"
					 #:type 'Card))

		 (define alice:library
		   (thing #:name "Alice's Library"
			  #:type 'Library
			  #:value (list fireball)))

		 (define bob:library
		   (thing #:name "Bob's Library"
			  #:type 'Library
			  #:value (list goblin mountain)))

		 (define g
		   (put-in (new-game)
			   bob
			   alice
			   fireball
			   goblin
			   mountain
			   alice:library
			   bob:library))

		 (describe-things-in g)

		 )
	)

#;
(require (submod "." MtG test))

(module VM racket/base
	(provide tick tick-thing my-output run my program program?
		 my-self
		 run-program)

	(require
	  (submod ".." games//relations)
	  (only-in racket/format ~v))

	(define-namespace-anchor a)
	(define ns (namespace-anchor->namespace a))

	(define (program code #:lang [lang #f])
	  (thing #:name "Program"
		 #:type 'Program
		 #:value (~v code)
		 'lang lang))

	(define (program? t)
	  (eq? 'Program (type #:of t)))

	(define (my k)
	  (what-is k #:of (my-self)))

	(define my-output (make-parameter #f))

	(define (tick g)
	  (define programs
	    (filter
	      (lambda (t) (eq? (type #:of t) 'Program))
	      (things-in g)))

	  (for ([p programs])
	       (tick-thing p)))

	(define (tick-thing p)
	  (parameterize ([my-output (what-is 'output #:of p)])
	    (define new-v (eval (read (open-input-string
					(substring (value #:of p) 1 ) ))
				ns))
	    (redescribe! p 'output
			 new-v)))

	(define (output #:of t)
	  (what-is 'output #:of t))

	(define my-self (make-parameter #f))

	(define (run-program p) ;Must be attatched to a parent thing
	  (define code
	    (value #:of p))

	  (define lang (what-is 'lang #:of p #:fallback #f))

	  (parameterize ([my-self (parent-of p)])
	    (define to-open (substring code 1))

	    (when lang
	      (dynamic-require lang #f))
	    (eval `(let ()
		     ,(when lang `(local-require ,lang))
		     ,(read (open-input-string to-open)))
		  ns)))

	(define (run k #:of t)
	  (define b (what-is k #:of t
			     #:fallback #f))

	  (when b
	    (define code
	      (if (thing? b)
		  (value #:of b) ;Should check type is 'Program?
		  b))

	    (define lang (and
			   (thing? b)
			   (what-is 'lang #:of b #:fallback #f)))

	    (parameterize ([my-self t])
	      (define to-open (substring code 1))

	      ;;(displayln t)

	      ;(writeln to-open)
	      ;(writeln lang)

	      ;(displayln (dynamic-require lang 'action:change-mana))

	      ;(displayln (eval '(action:change-mana #:of 'a #:by #'b)
	      ;                 (module->namespace '(submod nomic/gml/base CS))))

	      ;(displayln (read (open-input-string to-open)))

	      (when lang
		(dynamic-require lang #f))
	      (eval `(let ()
		       ,(when lang `(local-require ,lang))
		       ,(read (open-input-string to-open)))
		    ns
		    #;
		    (if lang
			(module->namespace lang)
			ns)))))


	(module+ test
		 (require rackunit
			  racket/list)



		 (define program
		   (thing #:name "Program"
			  #:type 'Program
			  #:value "'(+ 1 1)"
			  'output (void)
			  ))

		 (define g1
		   (put-in (new-game)
			   program))

		 (tick g1)

		 (check-equal? (what-is 'output #:of program)
			       2)

		 (require rackunit)


		 (define program2
		   (thing #:name "Program"
			  #:type 'Program
			  #:value "'(+ 5 (my-output))"
			  'output 5
			  ))

		 (define g2
		   (put-in (new-game)
			   program2))

		 (tick g2)

		 (check-equal? (what-is 'output #:of program2)
			       10)


		 (define program3
		   (thing #:name "Program"
			  #:type 'Program
			  #:value "'(put-in (my-output) (thing #:name \"HI\"))"
			  'output (new-game)
			  ))

		 (define g3
		   (put-in (new-game)
			   program3))

		 (tick g3)

		 (check-equal? (length
				 (things-in
				   (what-is 'output #:of program3)))
			       1)

		 (tick g3)

		 (check-equal? (length
				 (things-in
				   (what-is 'output #:of program3)))
			       2)

		 ;(print-game g3)
		 ;(describe-things-in (output #:of (first (things-in g3))))
		 )
	)

(require (submod "." VM test))


(module CS racket/base
	(provide action:change-mana do-action
		 (all-from-out (submod ".." games//relations)))

	(require
	  (submod ".." games//relations)
	  (submod ".." VM))


	(define (mana #:of t)
	  (what-is 'mana #:of t))

	(define (action:change-mana #:of t #:by a)
	  (thing #:name "Change Mana Action"
		 #:type 'Action
		 'target t
		 'effect 'redescribe!
		 'key 'mana
		 'val (+ (mana #:of t) a)))

	(define (do-action a)
	  (local-require racket/match)

	  (define e (what-is 'effect #:of a))
	  (define t (what-is 'target #:of a))
	  (define k (what-is 'key #:of a))
	  (define v (what-is 'val #:of a))

	  (match e
		 ['redescribe! (redescribe! t k v)]
		 [else (error "Not an allowed action: " e)])
	  )




	(module+ test
		 (require rackunit
			  racket/list racket/function racket/contract)

		 (define lindsey (thing #:name "Lindsey"
					#:type 'Player))

		 (define stephen (thing #:name "Stephen"
					#:type 'Player))

		 (define current-player (thing #:name "Current Player"
					       #:type 'CurrentPlayer
					       #:value stephen))

		 (define stephen:nexus (thing #:name "Stephen's Nexus"
					      #:type 'Permanent
					      'mana 100
					      'regen 25
					      'owner stephen
					      'on-turn-begin
					      (program
						#:lang '(submod nomic/gml/base CS)
						'(displayln (my-self))
						#;
						'(action:change-mana
						   #:of (my-self)
						   #:by (my 'regen)))))

		 (define lindsey:nexus (thing #:name "Lindsey's Nexus"
					      #:type 'Permanent
					      'mana 100
					      'regen 25
					      'owner lindsey
					      'on-turn-begin
					      (program
						#:lang '(submod nomic/gml/base CS)
						'(action:change-mana
						   #:of (my-self)
						   #:by (my 'regen)))))

		 (define lindsey:sevarog (thing #:name "Lindsey's Sevarog"
						#:type 'Creature
						'mana 100))

		 (define stephen:parasite (thing #:name "Stephen's Parasite"
						 #:type 'Creature
						 'mana 100
						 'regen 33
						 'target lindsey:sevarog
						 'on-turn-begin
						 (program
						   #:lang '(submod nomic/gml/base CS)

						   '(list
						      (action:change-mana
							#:of (my 'target)
							#:by (- (my 'regen)))
						      (action:change-mana
							#:of (my-self)
							#:by (my 'regen))))))

		 (define action-queue (thing #:name "Action Queue"
					     #:type 'Queue
					     'queue '()

					     'flush
					     (program 
					       #:lang '(submod nomic/gml/base CS)
					       '(let ()
						  (map do-action
						       (what-is 'queue
								#:of (my-self)))
						  (redescribe! (my-self) 'queue '())
						  ))))

		 (define g
		   (put-in (new-game)
			   stephen
			   lindsey
			   current-player
			   stephen:nexus
			   lindsey:nexus
			   lindsey:sevarog
			   stephen:parasite
			   action-queue))

		 ;TODO: Move extra-game logic like this into the game...


		 (define as
		   (filter (not/c void?)
			   (flatten
			     (map (lambda (t)
				    (run 'on-turn-begin #:of t))
				  (things-in g)))))


		 (redescribe! action-queue
			      'queue
			      as)

		 ;We've queued the changes, but they haven't run yet

		 (check-equal? (mana #:of lindsey:sevarog)
			       100)

		 (check-equal? (mana #:of stephen:parasite)
			       100)

		 ;Now, they will run...

		 (run 'flush #:of action-queue)

		 (check-equal? (mana #:of lindsey:sevarog)
			       67)

		 (check-equal? (mana #:of stephen:parasite)
			       133)




		 #|

		 (define Spell
		   (type Spell #:subtype (any-of 'Permanent 'Creature 'Enchantment)
			 #:state   (any-of 'in-play 'in-library)
			 #:condition (if (state-is 'in-play)
					 (can-have #:target (of-type 'Spell)))
			 #:content program?
			 #:value any?))


		 (define Player
		   (type 'Player
			 #:library (list-of Spell)
			 #:in-play (list-of Spell)))





		 |#



		 )
	)


(require (submod "." CS test))




