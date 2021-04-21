#lang racket
(require racket/draw)
(require "math.rkt")
(require "types.rkt")

(provide scanline
         divide-and-monty)


; Slow but accurate.
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


(define (random-color)
  (make-color
   (random 200)
   (random 200)
   (random 200)
   1.0))


(define (blur-bmp positive negative)
  ; positive is probably RGBA
  ; negative is probably monochrome

  (define start (current-inexact-milliseconds))

  ; Common image parameters for intermediaries.
  (define width (send positive get-width))
  (define height (send positive get-height))

  ; Intermediary where the positive will be accumulated w/ jitter.
  (define blur-bmp (make-bitmap width height))
  (define blur-ctx (new bitmap-dc% [bitmap blur-bmp]))
  (send blur-ctx set-background (make-color 0 0 0 0.0))
  (send blur-ctx clear)

  ; Weighting factor determined by the number of times the positive
  ; is drawn into blur-bpm
  (send blur-ctx set-alpha (/ 1 20))

  ; Accumulate the positive by some jitter amount.
  (define (blur amount)
    (define (blur-inner x y)
      (send blur-ctx draw-bitmap positive x y))
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

  ; Target for combining the postiive, negative, and blur-bmp images.
  (define final-bmp (make-bitmap width height))
  (define final-ctx (new bitmap-dc% [bitmap final-bmp]))

  ; Draw the blurred image masked by the negative to prevent drawing into
  ; regions where we know definitely are negative space.
  (send final-ctx draw-bitmap blur-bmp 0 0 'opaque (make-color 255 255 255) negative)

  ; Draw the positive image on top of the blurred image to preserve detail
  ; in the regions we know are definitely solid.
  (send final-ctx draw-bitmap positive 0 0)

  ; Print some stats.
  (define stop (current-inexact-milliseconds))
  (define delta (/ (round (- stop start)) 1000.0))
  (display "Blur took ")
  (display delta)
  (display " seconds.\n")
  final-bmp)


(define (divide-and-monty field)
  ; Parameters with reasonable defaults that probably don't need to be exposed anymore.
  (define tile-min 8)
  (define monty-iterations 1)
  (define random-split #t)
  (define last-is-random #t)

  ; Used for debugging.
  (define use-random-color #f)

  ; Used for profiling.
  (define iterations 0)
  (define draws 0)

  ; Common image parameters and starting tile extent.
  (define extent (aabb-flatten (field->aabb field)))
  (define width (+ 1 (exact-ceiling (aabb-width extent))))
  (define height (+ 1 (exact-ceiling (aabb-height extent))))

  ; Used to map spacial coordinates back to image coordinates.
  (define align (swiz (aabb-min extent) 0 1))

  ; Drawing context for dist <= 0 matches (solids).
  (define bmp (make-bitmap width height))
  (define ctx (new bitmap-dc% [bitmap bmp]))
  (send ctx set-smoothing 'smoothed)

  ; Drawing context for dist > 0 matches (empty space).
  (define mask-bmp (make-monochrome-bitmap width height))
  (define mask-ctx (new bitmap-dc% [bitmap mask-bmp]))
  (send mask-ctx set-pen (make-color 255 255 255) 0 'solid)
  (send mask-ctx set-brush (make-color 255 255 255) 'solid)
  (send mask-ctx set-background (make-color 0 0 0))
  (send mask-ctx clear)

  (define last-color #f)
  ; Sets the color for the primary drawing context.
  (define (set-color color)
    ; The redundant state elimination here makes a massive difference on perf.
    (unless (eq? color last-color)
      (set! last-color color)
      (send ctx set-pen color 0 'solid)
      (send ctx set-brush color 'solid)))

  ; Selects the tile splitting function.
  (define aabb-split-fn
    (if random-split
        aabb-split-random
        aabb-split))

  ; Selects the small tile sampling point function.
  (define point-fn (if last-is-random aabb-random aabb-center))

  ; Called when a tile can't be divided any further.  This will then sample a
  ; final point somewhere in the tile, and then either draw a circle in the
  ; solid bitmap or into the mask bitmap.
  (define (partial-coverage extent)
    (for ([i (in-range monty-iterations)])
      (define point (point-fn extent))
      (define-values (dist color) (sample-unpack (field point)))
      (define-values (img-x img-y) (vector->values (vector-sub (swiz point 0 1) align)))
      (set! draws (+ draws 1))
      (if (dist . <= . 0)
          (begin
            ; Draw into the solid bitmap.
            (when use-random-color
              (set! color (random-color)))
            (set-color color)
            (draw-circle ctx
                         (exact-floor img-x)
                         (exact-floor img-y)
                         (abs dist)))
          (begin
            ; Draw into the mask bitmap.
            (draw-circle mask-ctx
                         (exact-floor img-x)
                         (exact-floor img-y)
                         (abs dist))))))

  ; Called to split a tile in half or into fourths, and then recurse back into "search".
  ; If the tile can't be split any further, then this will call "partial-coverage" instead.
  (define (split extent)
    (define width (aabb-width extent))
    (define height (aabb-height extent))
    (define tiles
      (if (height . > . tile-min)
          ; split vertically
          (aabb-split-fn extent 1)
          (list extent)))
    (when (width . > . tile-min)
      ; split horizontally
      (set! tiles (apply append (map (lambda (extent) (aabb-split-fn extent 0)) tiles))))
    (if ((length tiles) . > . 1)
        (map search tiles)
        (partial-coverage extent)))

  ; Called to start drawing.  If the tile does not overlap an edge, then draw a circle in
  ; either the solid or mask image.  Otherwise, call "split" to try to divide into more
  ; tiles and recurse.
  (define (search extent)
    (set! iterations (+ iterations 1))
    (define tile-radius (aabb-radius extent))
    (define tile-center (aabb-center extent))
    (define-values (dist color) (sample-unpack (field tile-center)))
    (define-values (img-x img-y) (vector->values (vector-sub (swiz tile-center 0 1) align)))
    (cond
      [(and (dist . <= . 0) ((abs dist) . >= . tile-radius))
       ; Tile is entirely within a solid.
       (set! draws (+ draws 1))
       (when use-random-color
         (set! color (random-color)))
       (set-color color)
       (draw-circle ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius (abs dist)))]
      [(dist . >= . tile-radius)
       ; Tile overlaps nothing.
       (set! draws (+ draws 1))
       (draw-circle mask-ctx
                    (exact-floor img-x)
                    (exact-floor img-y)
                    (min tile-radius dist))]
      [else
       ; Tile overlaps an edge.
       (split extent)]))

  ; Generate the positive and negative images, and print some stats.
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

  ; Fill the "low confidence" area between the positive and negative images
  ; by blurring the positive into the non-masked area.
  (blur-bmp bmp mask-bmp))
