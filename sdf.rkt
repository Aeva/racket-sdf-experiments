#lang racket

(require "math.rkt")
(require "types.rkt")
(require "draw.rkt")


; test code
(monty
 (scale
  (cut
   (union (sphere #(0 0 0) 40)
          (sphere #(40 0 0) 20)
          (sphere #(60 0 0) 10)
          (sphere #(70 0 0) 7))
   (sphere #(-20 0 0) 30))
  4) 50000)