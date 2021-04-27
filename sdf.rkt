#lang racket

(require racket/draw)
(require racket/format)
(require "math.rkt")
(require "types.rkt")
(require "draw.rkt")


(define (named-color name)
  (send the-color-database find-color "orange"))


; test code
(define test-field
  (scale
   (cut
    (union (sphere #(0 0 0) 40)
           (sphere #(40 0 0) 20)
           (colorize (sphere #(60 0 0) 10) "orange")
           (sphere #(70 0 0) 7))
    (sphere #(-20 0 10) 30))
   4))


(define (test-field2 spin)
  (rotate-field
   (rotate-field
    (union
     (cut
      (rotate-field
       (cube #(0 0 0) #(100 100 100))
       'z (degrees->radians -25))
      (sphere #(0 0 0) 120))
     (colorize (sphere #(0 0 0) 70) "violet"))
    'x (degrees->radians 67))
   'y (degrees->radians spin)))


(define (test-frame (frame 0))
  (orthographic-box (test-field2 frame) 5))

(test-frame 18)
;(animate 20 test-frame)
         