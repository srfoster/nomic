#lang at-exp codespells

(provide find-by-name-in-radius held-thing change-text)

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