#lang racket
(require racket/draw)
(require "types.rkt")

(provide bisect
         monty)


(define (bisect field)
  (define x-extent (vector-ref (field-extent field) 0))
  (define y-extent (vector-ref (field-extent field) 1))
  (define width (+ 1 (* x-extent 2)))
  (define height (+ 1 (* y-extent 2)))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))

  (define scan-start #f)
  (define scan-stop #f)
  (define (draw y)
    (send ctx draw-line
          scan-start y
          scan-stop y)
    (set! scan-start #f))

  (for ([y (in-range height)])
    (for ([x (in-range width)])
      (define sample-x (- x x-extent))
      (define sample-y (- y y-extent))
      (define sample-at (vector sample-x sample-y 0))
      (define sample (field sample-at))
      (define solid (sample . <= . 0))
      (cond
        [(and solid scan-start)
         (set! scan-stop x)]
        [(and solid (not scan-start))
         (set! scan-start x)
         (set! scan-stop x)]
        [(and (not solid) scan-start)
         (draw y)]
        [else (void)]))
    (when scan-start (draw y)))
  bmp)


(define (monty field (iterations 10000) (max-radius +inf.f))
  (define x-extent (vector-ref (field-extent field) 0))
  (define y-extent (vector-ref (field-extent field) 1))
  (define width (+ 1 (* x-extent 2)))
  (define height (+ 1 (* y-extent 2)))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (for ([i (in-range iterations)])
    (define x (round (* width (random))))
    (define y (round (* height (random))))
    (define sample-x (- x x-extent))
    (define sample-y (- y y-extent))
    (define sample-at (vector sample-x sample-y 0))
    (define-values (dist color) (sample-unpack (field sample-at)))
    (send ctx set-pen color 0 'solid)
    (send ctx set-brush color 'solid)
    (when (dist . < . 0)
      (define r (min (abs dist) max-radius))
      (send ctx draw-ellipse (- x r) (- y r) (* r 2) (* r 2))))
  bmp)
