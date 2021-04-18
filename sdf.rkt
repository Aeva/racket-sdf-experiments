#lang racket

(require "math.rkt")
(require "types.rkt")
(require "draw.rkt")


;; test code
(bisect
 (scale
  (union (sphere #(0 0 0) 40)
         (sphere #(40 0 0) 20)
         (sphere #(60 0 0) 10)
         (sphere #(70 0 0) 7))
 4))
