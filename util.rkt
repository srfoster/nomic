#lang at-exp codespells

(provide target-permanent)

; Might need to traverse the actor tree up to the root and use a smaller radius
(define (target-permanent)
  (unreal-js
   @~a{(function(){
  var actors = KismetSystemLibrary.SphereOverlapActors(GWorld, {X:@(current-x), Y:@(current-z), Z:@(current-y)}, 100).OutActors
  var tagged = Object.values(global.taggedThings).filter((t) => actors.indexOf(t) > -1);
  return tagged[0];
  })()}))
