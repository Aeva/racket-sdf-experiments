#lang racket
(require racket/draw)
(require "math.rkt")
(require "types.rkt")

(provide scanline
         monty
         quad-search
         divide-and-monty)


(define (scanline field)
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
      (define sample (sample-dist (field sample-at)))
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


(define (draw-circle ctx x y r)
  (send ctx draw-ellipse (- x r) (- y r) (* r 2) (* r 2)))


(define (monty field (iterations 10000) (max-radius +inf.f))
  (define extent (aabb-flatten (field->aabb field)))
  (define width (+ 1 (exact-round (aabb-width extent))))
  (define height (+ 1 (exact-round (aabb-height extent))))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (define last-color #f)
  (for ([i (in-range iterations)])
    (define img-x (round (* width (random))))
    (define img-y (round (* height (random))))
    (define point
      (vector-mad #(1 1 0) (aabb-min extent) (vector img-x img-y 0)))
    (define-values (dist color) (sample-unpack (field point)))
    (unless (eq? color last-color)
      (send ctx set-pen color 0 'solid)
      (send ctx set-brush color 'solid)
      (set! last-color color))
    (when (dist . < . 0)
      (define r (min (abs dist) max-radius))
      (draw-circle ctx img-x img-y r)))
  bmp)


(define (random-color)
  (make-color
   (random 200)
   (random 200)
   (random 200)
   1.0))


(define (quad-search field (tile-min 1) (use-random-color #f))
  (define extent (aabb-flatten (field->aabb field)))
  (define width (+ 1 (exact-ceiling (aabb-width extent))))
  (define height (+ 1 (exact-ceiling (aabb-height extent))))
  (define align (swiz (aabb-min extent) 0 1))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (define iterations 0)
  (define draws 0)

  (define (split extent)
    (define width (aabb-width extent))
    (define height (aabb-height extent))
    (define tiles
      (if (height . > . tile-min)
          (aabb-split extent 1)
          (list extent)))
    (when (width . > . tile-min)
      (set! tiles (apply append (map (lambda (extent) (aabb-split extent 0)) tiles))))
    (when ((length tiles) . > . 1)
      (map search tiles)))

  (define (search extent)
    (set! iterations (+ iterations 1))
    (define tile-radius (aabb-radius extent))
    (define tile-center (aabb-center extent))
    (define-values (dist color) (sample-unpack (field tile-center)))
    (cond
      [(and (dist . <= . 0) ((abs dist) . >= . tile-radius))
       (set! draws (+ draws 1))
       (define-values (img-x img-y) (vector->values (vector-sub (swiz tile-center 0 1) align)))
       (when use-random-color
         (set! color (random-color)))
       (send ctx set-pen color 0 'solid)
       (send ctx set-brush color 'solid)
       (draw-circle ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius (abs dist)))]
      [else (split extent)]))
  (search extent)
  (display "Binary search draw finished in ")(display iterations)(display " iterations and drew ")
  (display draws)(display " circles.\n")
  bmp)


(define (divide-and-monty field (tile-min 32) (monty-iterations 100) (use-random-color #f))
  (define extent (aabb-flatten (field->aabb field)))
  (define width (+ 1 (exact-ceiling (aabb-width extent))))
  (define height (+ 1 (exact-ceiling (aabb-height extent))))
  (define align (swiz (aabb-min extent) 0 1))
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (define iterations 0)
  (define draws 0)

  (define (monty extent)
    (for ([i (in-range monty-iterations)])
      (define point (aabb-random extent))
      (define-values (dist color) (sample-unpack (field point)))
      (when (dist . <= . 0)
        (set! draws (+ draws 1))
        (define-values (img-x img-y) (vector->values (vector-sub (swiz point 0 1) align)))
        (when use-random-color
          (set! color (random-color)))
        (send ctx set-pen color 0 'solid)
        (send ctx set-brush color 'solid)
        (draw-circle ctx
                     (exact-floor img-x)
                     (exact-floor img-y)
                     (abs dist)))))

  (define (split extent)
    (define width (aabb-width extent))
    (define height (aabb-height extent))
    (define tiles
      (if (height . > . tile-min)
          (aabb-split extent 1)
          (list extent)))
    (when (width . > . tile-min)
      (set! tiles (apply append (map (lambda (extent) (aabb-split extent 0)) tiles))))
    (if ((length tiles) . > . 1)
        (map search tiles)
        (monty extent)))

  (define (search extent)
    (set! iterations (+ iterations 1))
    (define tile-radius (aabb-radius extent))
    (define tile-center (aabb-center extent))
    (define-values (dist color) (sample-unpack (field tile-center)))
    (cond
      [(and (dist . <= . 0) ((abs dist) . >= . tile-radius))
       (set! draws (+ draws 1))
       (define-values (img-x img-y) (vector->values (vector-sub (swiz tile-center 0 1) align)))
       (when use-random-color
         (set! color (random-color)))
       (send ctx set-pen color 0 'solid)
       (send ctx set-brush color 'solid)
       (draw-circle ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius (abs dist)))]
      [else (split extent)]))
  (define start (current-inexact-milliseconds))
  (search extent)
  (define stop (current-inexact-milliseconds))
  (define delta (/ (round (- stop start)) 1000.0))
  (display "Binary search draw finished in ")
  (display delta)
  (display " seconds and took ")
  (display iterations)
  (display " iterations to draw ")
  (display draws)(display " circles.\n")
  bmp)
