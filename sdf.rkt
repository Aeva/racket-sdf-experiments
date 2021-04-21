#lang racket

(require racket/draw)
(require "math.rkt")
(require "types.rkt")
(require "draw.rkt")


(define (named-color name)
  (send the-color-database find-color "orange"))


; test code
(define test-field
  (pad-extent
   (scale
    (cut
     (union (sphere #(0 0 0) 40)
            (sphere #(40 0 0) 20)
            (colorize (sphere #(60 0 0) 10) "orange")
            (sphere #(70 0 0) 7))
     (sphere #(-20 0 10) 30))
    4) 10))

;(divide-and-monty test-field)
(orthographic-box test-field 10)
