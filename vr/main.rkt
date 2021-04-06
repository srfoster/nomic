#lang at-exp codespells


;Remember: What you provide out is available as a twitch command!
(provide (all-from-out nomic/new-twitch)
         cheer
         face
         color
         mini
         say)

(require-mod fire-particles)
(require-mod rocks)
(require-mod chess)
(require nomic/game/model
         nomic/new-twitch
         (submod nomic/gml/base games//relations)
         taggable
         hierarchy)

;In: S:\CodeSpellsWorkspace\Projects\cabin-world\Build\WindowsNoEditor> 
;Run: .\CodeSpellsDemoWorld.exe -unreal-server=8080 -codespells-server=8081

;(thread (demo-aether))

(map (lambda (f)
       (f))
     mod-setups)


(define (cheer)
  (hash 'type 'Cheer
        'twitch-id (current-twitch-id)))

(define (place-cheer td)
  (unreal-eval-js
   @unreal-js{
      (function(){
         var mini = @(find-with-tag (hash-ref td 'twitch-id));
         var cheer = @(explosion);
         console.log("+++++++++++", cheer);
         cheer.SetActorLocation(mini.GetActorLocation());
  })()}
   #;(at [640.0 186.0 -360.0]
       (explosion))))

(define (face name)
  (hash 'type 'Face
        'name name))

;(define (☺☺☺☺☺ x))

(define-syntax-rule (define-face face-name)
  (begin
    (provide face-name)
    (define face-name
      (~a 'face-name))
    ))

(define-syntax-rule (define-color color-name)
  (begin
    (provide color-name)
    (define color-name
      (~a 'color-name))))

(define-color red)
(define-color green)
(define-color blue)
(define-color orange)

(define-face bernie-sanders)
(define-face doge)
(define-face grumpy-cat)
(define-face polite-cat)
(define-face sad-pepe)
(define-face shocked-cat)
(define-face surprised-cat)
(define-face surprised-pikachu)
(define-face troll-face)
(define-face you-kidding-me)

(define (change-face name)
  (unreal-eval-js
   @unreal-js{
 var mcp = GWorld.GetAllActorsOfClass(Root.ResolveClass('VRAvatar')).OutActors[0]
 mcp.ChangeFace(Material.Load("/Game/Faces/@|name|_Mat"))
 }))

(define (color name)
  (displayln "Color change!!")
  (hash 'type 'Color
        'name name))

(define (change-color color)
  (unreal-eval-js
   @unreal-js{
 var mcp = GWorld.GetAllActorsOfClass(Root.ResolveClass('VRAvatar')).OutActors[0]
 mcp.ChangeColor(ParticleSystem.Load("/Game/Orbs/@|(string-titlecase color)|Orb"))
 }))

(define (mini) ;What are the allowed models?
  (hash 'type 'Miniature
        'twitch-id (current-twitch-id)))

(define (place-mini td)
  ;NOTE: This model (if it is a procedure that
  ; came from a sandboxed evaluator),
  ; can't be wrapped in (at [] ...) here. Not sure
  ; exactly why...  Maybe the current-x/y/z params
  ; behave weirdly with evaluators
  
  (define twitch-id (hash-ref td 'twitch-id))

  ;TODO: Figure out list of available minis
  ;TODO: Scale so it is small
  ;TODO: Place text above the mini
  ;TODO: Don't let them overlap.  Flocking??

  (define js
    (at [640.0 186.0 -360.0]
        (with-tag twitch-id
       @unreal-js{
         (function(){
         var Mini = Root.ResolveClass('PickupMini');
         var mini = new Mini(GWorld,{X: @(current-x), Y: @(current-z), Z: @(current-y)});
         mini.ChangeName('@twitch-id');

         return mini;
         })()
       })))

  (unreal-eval-js js))

(define (held-thing)
  (define js
    @unreal-js{
 (function(){
 var mcp = GWorld.GetAllActorsOfClass(Root.ResolveClass('VRAvatar')).OutActors[0]
 var htr = mcp.HeldThingRight().HeldThingRight;
 var htl = mcp.HeldThingLeft().HeldThingLeft;

 var ht = htr || htl;

 return ht
  })()
 })
  js)

(define (change-text a t)
  (define js
    @unreal-js{
 (function(){
 var a = @a;
 a.ChangeText(@(~s t));
  })()
 })
  (unreal-eval-js js))

;Only works when TB is holding something that has a ChangeText()
(define (say t)
  (change-text
   (held-thing)
   t))



(module+ main
  (serve-game (lambda (td)
                (cond
                  [(eq? (hash-ref td 'type) 'Cheer)
                   (place-cheer td)]
                  [(eq? (hash-ref td 'type) 'Face)
                   (change-face (hash-ref td 'name))]
                  [(eq? (hash-ref td 'type) 'Color)
                   (change-color (hash-ref td 'name))]
                  [(eq? (hash-ref td 'type) 'Miniature)
                   (place-mini td)]
                  [else (error "What was that?")]))
              'nomic/vr/main))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (run-remote-spell id)
  (local-require net/http-easy
                 json)
  (define res
    (get
     (~a "https://guarded-mesa-01320.herokuapp.com/secret/"
         id)))
  (define payload
    (response-json res))
  (define code-string
    (~a
     "(let ()"
     (hash-ref payload 'text)
     ")"))
  (define code
    (read (open-input-string code-string)))
  
  (eval code ns))

(provide run-staged)
(define (run-staged)
  (run-remote-spell 22))
#;(define (run-staged)
  (change-color "green")

  (define pause 2)

  (define messages
    (list
     @~a{Is this thing on?}
     @~a{Okay, here goes...}

     @~a{Hi, I'm CodeSpells}
     @~a{Yes, I know}
     @~a{I look like a cat}

     @~a{But I'm no ordinary cat}
     @~a{I have a YouTube...}
     @~a{And a Twitch...}

     @~a{And unlike other cats}
     ;Pull cat memes in from off screen
     ;  Like this one: ___ 
     ;  Or this one: ___
     ;  Or even this one: ___ [polite cat]

     ;Longer pause before this...
     @~a{I am more than a meme!}

     ; Hmm. What next.  how to explain show concept?
     ;   Do we need to do any storytelling related to
     ;   the old direction?
     ; Maybe just link to a blog post for more info...
     

     ;I'm what happens when you take a game about
     ;coding magic spells, and try to explain it
     ;to the internet.


     ;Magic of coding, made interesting and weird...

     ; Thank you for joining me for breakfast
 
     ))

  (for ([s messages])
    (sleep pause)
    (change-text
     (held-thing)
     s)))