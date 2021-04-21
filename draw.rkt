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
  (send ctx set-background (make-color 0 0 0 0.0))
  (send ctx clear)
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


(define (divide-and-monty field (tile-min 32) (monty-iterations 100) (random-split #t) (last-is-random #f) (use-random-color #f) (clip-tiles #f))
  (define extent (aabb-flatten (field->aabb field)))
  (define width (+ 1 (exact-ceiling (aabb-width extent))))
  (define height (+ 1 (exact-ceiling (aabb-height extent))))
  (define align (swiz (aabb-min extent) 0 1))
  (define iterations 0)
  (define draws 0)

  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (send ctx set-smoothing 'smoothed)

  (define mask-bmp (make-monochrome-bitmap width height))
  (define mask-ctx (new bitmap-dc% [bitmap mask-bmp]))
  (send mask-ctx set-pen (make-color 255 255 255) 0 'solid)
  (send mask-ctx set-brush (make-color 255 255 255) 'solid)
  (send mask-ctx set-background (make-color 0 0 0))
  (send mask-ctx clear)
  
  (define aabb-split-fn
    (if random-split
        aabb-split-random
        aabb-split))
  
  (define last-color #f)
  (define (set-color color)
    (unless (eq? color last-color)
      (set! last-color color)
      (send ctx set-pen color 0 'solid)
      (send ctx set-brush color 'solid)))

  (define (clip extent)
    (define-values (x y) (vector->values (vector-sub (swiz (aabb-min extent) 0 1) align)))
    (define w (aabb-width extent))
    (define h (aabb-height extent))
    (send ctx set-clipping-rect (floor x) (floor y) (ceiling w) (ceiling h)))

  (define (monty extent)
    (when clip-tiles
      (clip extent))
    (for ([i (in-range monty-iterations)])
      (define point (if last-is-random (aabb-random extent) (aabb-center extent)))
      (define-values (dist color) (sample-unpack (field point)))
      (define-values (img-x img-y) (vector->values (vector-sub (swiz point 0 1) align)))
      (set! draws (+ draws 1))
      (if (dist . <= . 0)
          (begin
            (when use-random-color
              (set! color (random-color)))
            (set-color color)
            (draw-circle ctx
                         (exact-floor img-x)
                         (exact-floor img-y)
                         (abs dist)))
          (begin
            (draw-circle mask-ctx
                         (exact-floor img-x)
                         (exact-floor img-y)
                         (abs dist))))))

  (define (split extent)
    (define width (aabb-width extent))
    (define height (aabb-height extent))
    (define tiles
      (if (height . > . tile-min)
          (aabb-split-fn extent 1)
          (list extent)))
    (when (width . > . tile-min)
      (set! tiles (apply append (map (lambda (extent) (aabb-split-fn extent 0)) tiles))))
    (if ((length tiles) . > . 1)
        (map search tiles)
        (monty extent)))

  (define (search extent)
    (set! iterations (+ iterations 1))
    (define tile-radius (aabb-radius extent))
    (define tile-center (aabb-center extent))
    (define-values (dist color) (sample-unpack (field tile-center)))
    (define-values (img-x img-y) (vector->values (vector-sub (swiz tile-center 0 1) align)))
    (cond
      [(and (dist . <= . 0) ((abs dist) . >= . tile-radius))
       (set! draws (+ draws 1))
       (when use-random-color
         (set! color (random-color)))
       (set-color color)
       (when clip-tiles
         (clip extent))
       (draw-circle ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius (abs dist)))]
      [(dist . >= . tile-radius)
       (set! draws (+ draws 1))
       (draw-circle mask-ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius dist))]
      [else (split extent)]))
  (define start (current-inexact-milliseconds))
  (search extent)
  (define stop (current-inexact-milliseconds))
  (define delta (/ (round (- stop start)) 1000.0))
  (display "Draw finished in ")
  (display delta)
  (display " seconds and took ")
  (display iterations)
  (display " iterations to draw ")
  (display draws)(display " circles.\n")

  (define blur-start (current-inexact-milliseconds))
  (define blur-bmp (make-bitmap width height))
  (define blur-ctx (new bitmap-dc% [bitmap blur-bmp]))
  (send blur-ctx set-background (make-color 0 0 0 0.0))
  (send blur-ctx clear)
  (send blur-ctx set-alpha (/ 1 20))

  (define (blur amount)
    (define (blur-inner x y)
      (send blur-ctx draw-bitmap bmp x y 'opaque (make-color 255 255 255)))
    (define pos amount)
    (define neg (* -1 pos))
    (blur-inner neg 0)
    (blur-inner pos 0)
    (blur-inner 0 neg)
    (blur-inner 0 pos))
  (blur 1)
  (blur 2)
  (blur 4)
  (blur 8)
  (blur 16)

  (define final-bmp (make-bitmap width height))
  (define final-ctx (new bitmap-dc% [bitmap final-bmp]))
  (send final-ctx draw-bitmap blur-bmp 0 0 'opaque (make-color 255 255 255) mask-bmp)
  (send final-ctx draw-bitmap bmp 0 0)
  (define blur-stop (current-inexact-milliseconds))
  (define blur-delta (/ (round (- blur-stop blur-start)) 1000.0))
  (display "Blur took ")
  (display blur-delta)
  (display " seconds.\n")

  final-bmp)
