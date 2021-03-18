#lang racket

(define (crystal-with-circle)
  (parentify
   (crystals dark-crystal)
   (scale
    (xyz-vector 3 3 3)
    basic-circle)))