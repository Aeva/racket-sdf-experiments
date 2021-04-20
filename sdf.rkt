#lang racket

(require racket/draw)
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
          (colorize (sphere #(40 0 0) 20) (named-color "orange"))
          (sphere #(60 0 0) 10)
          (sphere #(70 0 0) 7))
   (sphere #(-20 0 0) 30))
  4))

(divide-and-monty test-field 32 50 #t #f #f)
