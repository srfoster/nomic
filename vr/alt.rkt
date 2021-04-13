#lang at-exp codespells

#|

TODO:
Optimization: Put immovable target where things can drift to

Maybe we need to make "pickup" be a physics operation, tractor beam...
  - But then we'll have issues teleporting while holding things...
  - Can we disable gravity on assets like Apple and Pawn?

Problems: Things from mods (log cabin, rocks) are not spawning
    in reliably.
    - Apple: VR controlled: No, Normal: No
      * Exported as: Sphere Collider > Static Mesh
    - Gnarly Rock: VR controlled: No, Normal: Yes
      * Exported as: Static Mesh
    - Dark Pawn: VR controlled: Yes (but hacky offset?), Normal: Yes
      * Exported as: Static Mesh (Extending StaticMeshActor)

    - Crystal: VR controlled: Yes (but wacky physics), Normal: ??
    - Flames: VR controlled: Yes (but weird scale?), Normal: ??


Thunks are very confusing.  When do they work?  When not?

Fails:
@summon-off-camera[(with-tag "lindsey"
                     (disable-gravity
                        apple))]

Succeeds:
@summon-off-camera[(with-tag "lindsey"
                     (thunk (disable-gravity
                        apple))]


Can we do a trick where an apple drifts in and I catch it
  in the real world?
  - Alternative trailer video concept?







Efficency side-quest:
* Make spawned (off camera) things drift into frame
  - Write (drift ...) - Need to test
  - Need target to drift to
    * Write (find-by-name ...) 

I want to summon vr controllable Lindsey and Stephen cards
for the trailer video.  These will be web browsers.
* But I need to be able to set their URL.
* Maybe Make some chess-related content pitched at CodeMiko/BotezLive
   - Spawn in chess pieces
   - Spawn in relevant video clips from the streamers...

For the YT trailer video:
* Be able to spawn in more PickupCards
* Be able to say what's on them
  - Web browser cards?
  - Texture (/w cutout) -- for S + L cards
  - Text

|#

;In: S:\CodeSpellsWorkspace\Projects\cabin-world\Build\WindowsNoEditor> 
;Run: .\CodeSpellsDemoWorld.exe -unreal-server=8080 -codespells-server=8081

(require nomic/vr/main
         nomic/vr/util
         nomic/game/model

         hierarchy
         )



(define (find-by-name-in-radius name radius)
  @unreal-value{
     let actors = KismetSystemLibrary.SphereOverlapActors(GWorld, {X:@(current-x), Y:@(current-z), Z:@(current-y)}, @radius).OutActors

     for(let i = 0; i < actors.length; i++){
       let current = actors[i];

       if(current.toString().match("@|name|_C")){
         return current;
       }
     }

     return false;
 })

(define (move-to thing-1 thing-2)
  @unreal-value{
    var thing1 = @thing-1;
    var thing2 = @thing-2;
    thing1.SetActorLocation(thing2.GetActorLocation());

    return thing1;
 })

(define (drift
         #:force [f 1000]
         thing-1 thing-2)
   @unreal-value{
 var thing1 = @thing-1;
 var thing2 = @thing-2;
 var thing1Coords = thing1.GetActorLocation();
 var thing2Coords = thing2.GetActorLocation();
 var vect = {X: (thing2Coords.X - thing1Coords.X),
             Y: (thing2Coords.Y - thing1Coords.Y),
             Z: (thing2Coords.Z - thing1Coords.Z)};
 var magnitude = Math.sqrt(Math.pow(vect.X,2) +
                           Math.pow(vect.Y,2) +
                           Math.pow(vect.X,2))
 var vect2 = {X: (vect.X / magnitude) * @f,
              Y: (vect.Y / magnitude) * @f,
              Z: (vect.Z / magnitude) * @f};
 //thing1.AddForce(vect2);

 console.log(thing1.StaticMeshComponent.AddImpulse);
 
 thing1.StaticMeshComponent.AddImpulse(vect2);

 return thing1
 })

(define (browser-card url) ;Is everything a function?
  ;TODO: Make this browser load up Lindsey's face...
 @unreal-value{
   var card = @(spawn (class-from-exported "SimpleBrowser"));

   var Widget = card.GetComponentByClass(WidgetComponent);

   Widget.GetUserWidgetObject().WebBrowser.InitialURL = "@url"
              
   card.BrowseTo("@url");           

   return card;
 })

(define (lindsey-card)
 (browser-card "https://lindseyhandley.com"))

(define (stephen-card)
 (browser-card "https://stephenfoster.us"))

(define (laura-card)
 (browser-card "https://lauraheppell.github.io"))


(define (disable-gravity x)
  @unreal-value{
    var x = @x;

    console.log(x);
    var cs = x.GetComponentsByClass(StaticMeshComponent);
    console.log(cs);

    for(var i = 0; i < cs; i++){
      cs[i].SetEnableGravity(false);
    }
            
    return x;
 })

(define (summon-off-camera to-summon)
  (try
   (unreal-eval-js
    @unreal-js{
 (function(){
  var vrc = @(find-by-name-in-radius "PickupCamera" 1000)

  var l = vrc.GetActorLocation();
  
  var summoned = @(at ;Strings are gross here.
                   ["l.X"
                    "l.Z + 25"
                    "l.Y"]
                   (to-summon));

  return summoned
  })()
 })
   
   #;
   (unreal-eval-js
    @unreal-js{
 (function(){
  var vrc = GWorld.GetAllActorsOfClass(Root.ResolveClass('VRCamera')).OutActors[0]

  var summoned = @(to-summon);

  var vrcL = vrc.GetActorLocation();
 
  summoned.SetActorLocation({X: vrcL.X, Y: vrcL.Y, Z: vrcL.Z + 25});

  return summoned
  })()
 }))
  (sleep 1)
  (dump-logs))

(define-syntax-rule (try lines ...)
  (with-handlers 
      ([exn:fail?
        (λ (e) (displayln e))])
    lines ...
    ))

(define (dump-logs [n 10])
  (displayln
   (reverse
    (take
     (reverse
      (file->lines
       "S:\\CodeSpellsWorkspace\\Projects\\cabin-world\\Build\\WindowsNoEditor\\LogCabinWorld\\Saved\\Logs\\LogCabinWorld.log"
       ))
     n))))

(define (vr-controlled to-summon)
  (thunk
  @unreal-js{
 (function(){
  const uclass = require('uclass')().bind(this,global);
  class Thing extends Root.ResolveClass('PickupThing') {
                                                     
  }      
  let Thing_C = uclass(Thing);

  var thing = new Thing_C(GWorld,{X:@(current-x), Y:@(current-z), Z:@(current-y)});
  
  var child = @(at [0 75 0] (to-summon));
  if(child.StaticMeshComponent){
    child.StaticMeshComponent.SetSimulatePhysics(false);
    child.StaticMeshComponent.SetGenerateOverlapEvents(false);
  }
    
  @(parentify
    @unreal-js{thing}
    @unreal-js{child});
                   
  return thing;
  })()
 }))



(provide main)
(define (main)
  (displayln "Main!")

  (define (t s)
    (change-text
     (held-thing)
     s)

    (sleep 1))


  
;  @t{Yes}
;  @t{I know}
;  @t{I am a cat}

;  @t{But also...}
;  @t{I am an "avatar"}
  
  ;@t{For these two}

  ;@summon-off-camera[card-of-lindsey] @sleep[2]
  @summon-off-camera[(vr-controlled lindsey-card)]
  @sleep[2]

 ; @summon-off-camera[(vr-controlled stephen-card)]
 ; @sleep[2]

  ;@sleep[2] ;Wait for pull in

  ;@t{They are coders}



  
  #|
Yes
I know
I’m a cat

But 
I’m also an “avatar”,
For these two [Pull in Lindsey and Stephen cards]

They are coders 
who wrote a book called 
“Don’t Teach Coding” [Pull in book cover]

And a platform called
CodeSpells

And they believe
It’s time to
Use CodeSpells to talk about
Things they couldn’t address in their book

Text is limiting.

So join me.
The cat.
On Twitch.
And YouTube.

To discuss... [Music shift][Pull in various cards/media:]

Coding. Games. Education. Science. Magic. Memes. Media. SciFi. Fantasy. Lisp. [TODO: How many?]

[Wipe/explode all away with bomb?] [Pull in two final cards:]
Coding.
Meta coding.
|#




  )


(require taggable)

(module+ main
  (serve-game
   (lambda (td)
     (void)))


  @summon-off-camera[(with-tag "lindsey"
                       
                       (thunk (disable-gravity
                        apple)))]
  

#;
  @summon-off-camera[(with-tag "lindsey"
                       
                       (vr-controlled
                        ;apple
                        ;dark-pawn
                        ;lindsey-card
                        dark-crystal
                        #;
                        (thunk
                         (browser-card
                          "https://youtu.be/AnJiAByUBuU"))
                      
                        ))]

  #;
  (try
   (unreal-eval-js
    (drift (find-with-tag "lindsey")
           (find-by-name-in-radius "PickupBread" 1000))))


  
  
  (dump-logs)
  )

