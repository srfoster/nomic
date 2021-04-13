#lang at-exp codespells

;Remember: What you provide out is available as a twitch command!
(provide (all-from-out nomic/new-twitch)
         (all-defined-out))

(require-mod fire-particles)
(require-mod rocks)
(require-mod chess)
(require-mod crystals)
(require-mod cabin-items)

(require nomic/game/model
         nomic/new-twitch
         racket/sandbox
         (submod nomic/gml/base games//relations)
         taggable
         hierarchy
         nomic/vr/util)

;In: S:\CodeSpellsWorkspace\Projects\cabin-world\Build\WindowsNoEditor> 
;Run: .\CodeSpellsDemoWorld.exe -unreal-server=8080 -codespells-server=8081

;(thread (demo-aether))

(map (lambda (f)
       (f))
     mod-setups)

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

(define (add-force td)
  (define x (hash-ref td 'x))
  (define y (hash-ref td 'y))
  (define z (hash-ref td 'z))
  (unreal-eval-js
   @unreal-js{
 var mini = @(find-with-tag (hash-ref td 'twitch-id));
 mini.AddForce({X:@x,Y:@y,Z:@z})
 }))

(define (add-force-to td)
  (define f (hash-ref td 'force))
  (define tag (hash-ref td 'tag))
  (unreal-eval-js
   @unreal-js{
 var mini = @(find-with-tag (hash-ref td 'twitch-id));
 var obj = @(find-with-tag tag);
 var miniCoords = mini.GetActorLocation();
 var objCoords = obj.GetActorLocation();
 var vect = {X: (objCoords.X - miniCoords.X),
             Y: (objCoords.Y - miniCoords.Y),
             Z: (objCoords.Z - miniCoords.Z)};
 var magnitude = Math.sqrt(Math.pow(vect.X,2) +
                           Math.pow(vect.Y,2) +
                           Math.pow(vect.X,2))
 var vect2 = {X: (vect.X / magnitude) * @f,
              Y: (vect.Y / magnitude) * @f,
              Z: (vect.Z / magnitude) * @f};
 mini.AddForce(vect2);
 }))

(define (change-face name)
  (unreal-eval-js
   @unreal-js{
 var mcp = GWorld.GetAllActorsOfClass(Root.ResolveClass('VRAvatar')).OutActors[0]
 mcp.ChangeFace(Material.Load("/Game/Faces/@|name|_Mat"))
 }))

(define (change-color td)
  (unreal-eval-js
   @unreal-js{
 var mini = @(find-with-tag (hash-ref td 'twitch-id));
 mini.ChangeColor(ParticleSystem.Load("/Game/Orbs/@|(string-titlecase (hash-ref td 'name))|Orb"))
 }))

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
       (scale (xyz-vector 0.1 0.1 0.1) @unreal-js{
         (function(){
         var Mini = Root.ResolveClass('PickupMini');
         var mini = new Mini(GWorld,{X: @(current-x), Y: @(current-z), Z: @(current-y)});
         mini.ChangeName('@twitch-id');

         return mini;
         })()
       }))))

  (unreal-eval-js js))

;Only works when TB is holding something that has a ChangeText()
(define (do-say str)
  (change-text
   (held-thing)
   str))

(define interpret
  (lambda (td)
    (cond
      [(eq? (hash-ref td 'type) 'Cheer)
       (place-cheer td)]
      [(eq? (hash-ref td 'type) 'Face)
       (change-face (hash-ref td 'name))]
      [(eq? (hash-ref td 'type) 'Say)
       (do-say (hash-ref td 'message))]
      [(eq? (hash-ref td 'type) 'Color)
       (change-color td)]
      [(eq? (hash-ref td 'type) 'Miniature)
       (place-mini td)]
      [(eq? (hash-ref td 'type) 'Run)
       (run-remote-spell td)]
      [(eq? (hash-ref td 'type) 'Sleep)
       (sleep (hash-ref td 'seconds))]
      [(eq? (hash-ref td 'type) 'Force)
       (add-force td)]
      [(eq? (hash-ref td 'type) 'ForceTo)
       (add-force-to td)]
      [else (error "What was that?")])))

(module+ main
  
  (serve-game interpret
              'codespells-live/chat)

  (unreal-eval-js
   (tag "bread" (find-by-name-in-radius "PickupBread" 1000)))
  (unreal-eval-js
   (tag "kettle" (find-by-name-in-radius "PickupKettle" 1000)))
  (unreal-eval-js
   (tag "cup" (find-by-name-in-radius "PickupCup" 1000)))
  (unreal-eval-js
   (tag "TB" (find-by-name-in-radius "MotionControllerPawn" 1000)))
  (unreal-eval-js
   (tag "door" (find-by-name-in-radius "Door" 1000)))
  (unreal-eval-js
   (tag "furnace" (find-by-name-in-radius "BP_Furnace" 1000)))
  (unreal-eval-js
   (tag "camera1" (find-by-name-in-radius "PickupCamera" 1000)))
  (unreal-eval-js
   (tag "camera2" (find-by-name-in-radius "PickupCamera2" 1000)))
  )

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (run-remote-spell td)
  (local-require net/http-easy
                 json)

  (define id (hash-ref td 'id))
  (define twitch-id (hash-ref td 'twitch-id))
  
  (define res
    (get
     (~a "https://guarded-mesa-01320.herokuapp.com/secret/"
         id)))
  (define payload
    (response-json res))
  (define code-string
    (~a
     "(list "
     (hash-ref payload 'text)
     ")"))
  (define code
    (read (open-input-string code-string)))

  (dynamic-require 'codespells-live #f)
  (define instruction-list
    (eval `(with-twitch-id ,twitch-id
             ,code)
          (module->namespace 'codespells-live)))

  (displayln instruction-list)
  (map interpret instruction-list)
  
  #;(define safe-evaluator
    (call-with-trusted-sandbox-configuration
     (lambda ()
       (make-evaluator
        'codespells-live))))
  
  #;(safe-evaluator
   `(with-twitch-id ,twitch-id
      ,code))
  )



(provide run-staged)
(define (run-staged)
  (run-remote-spell 22))
