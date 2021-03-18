#lang at-exp racket

(provide with-tag
         move-with-tag
         delete-with-tag
         find-with-tag
         target-permanent)

(require codespells)

; Might need to traverse the actor tree up to the root and use a smaller radius
(define (target-permanent)
  (unreal-js
   @~a{(function(){
  var actors = KismetSystemLibrary.SphereOverlapActors(GWorld, {X:@(current-x), Y:@(current-z), Z:@(current-y)}, 100).OutActors
  var tagged = Object.values(global.taggedThings).filter((t) => actors.indexOf(t) > -1);
  return tagged[0];
  })()}))

;Should delete if already some
(define (with-tag tag code)
  (thunk
   @unreal-js{
     (function(){
     var x = @(if (procedure? code)
                  (code)
                  code)

     if(!global.taggedThings){
       global.taggedThings = {};}

     var destroy = function(a){
         a.GetAttachedActors().OutActors.map(destroy)

         a.DestroyActor()                         
     }

     if(global.taggedThings['@tag'])
       destroy(global.taggedThings['@tag']);
     
     global.taggedThings['@tag'] = x;
     return x;
     })()
 }))

(define (delete-with-tag tag)
  (thunk
   @unreal-js{
     (function(){
       var x = global.taggedThings["@tag"];
       console.log(x);

       var destroy = function(a){
         a.GetAttachedActors().OutActors.map(destroy)

         a.DestroyActor()                         
       }
       
       destroy(x);
       
       return x;
     })()
 }))

(define (move-with-tag tag)
  (thunk
   @unreal-js{
     (function(){
       var x = global.taggedThings["@tag"];

       x.SetActorLocation({X: @(current-x), Y: @(current-z), Z: @(current-y)})
                                    
       return x;
     })()
 }))

(define (find-with-tag tag)
  (thunk
   @unreal-js{
     (function(){
       var x = global.taggedThings["@tag"];
                                    
       return x;
     })()
 }))

  