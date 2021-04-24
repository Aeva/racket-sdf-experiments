#lang racket

(provide swiz
         vector-op
         vector-add
         vector-sub
         vector-mul
         vector-div
         vector-mad
         vector-min
         vector-max
         vector-map
         dot
         distance
         vector-mag
         lerp
         vector-lerp
         vector-rotate)


; Helper macro for swiz and swiz-acc.  This translates common
; lane names to their equivalent index values.
(define-syntax lane-id
  (syntax-rules (x y z w r g b a)
    [(lane-id x) 0]
    [(lane-id y) 1]
    [(lane-id z) 2]
    [(lane-id w) 3]
    [(lane-id r) 0]
    [(lane-id g) 1]
    [(lane-id b) 2]
    [(lane-id a) 3]
    [(lane-id fnord) fnord]))


; Helper macro for swiz.  This recursively expands the lanes into
; vector-ref calls, and then passes them into a new vector.
(define-syntax swiz-acc
  (syntax-rules ()
    [(swiz-acc (a ...)) (vector a ...)]
    [(swiz-acc (a ...) vec lane) (swiz-acc (a ... (vector-ref vec (lane-id lane))))]
    [(swiz-acc (a ...) vec lane lanes ...) (swiz-acc (a ... (vector-ref vec (lane-id lane))) vec lanes ...)]))


; Shader style swizzling for vectors.
; Usage examples:
;
; > (swiz #(0. 1.) x x x y)
; (vector 0.0 0.0 0.0 1.0)
;
; > (define myvec (vector .1 .2 .3 .4))
; > (swiz myvec x z w y)
; (vector 0.1 0.3 0.4 0.2)
(define-syntax swiz
  (syntax-rules (vector)
    [(swiz (vector data ...) lanes ...) (swiz #(data ...) lanes ...)]
    [(swiz #(data ...) lanes ...) (let ([vec (vector data ...)]) (swiz vec lanes ...))]
    [(swiz vec lane) (swiz-acc ((vector-ref vec (lane-id lane))))]
    [(swiz vec lane lanes ...) (swiz-acc ((vector-ref vec (lane-id lane))) vec lanes ...)]))


; Lane-wise vector operators
(define (vector-op op lhs rhs)
  (for/vector #:length (vector-length lhs) ([a lhs][b rhs])
    (op a b)))


(define (vector-add lhs rhs)
  (vector-op + lhs rhs))


(define (vector-sub lhs rhs)
  (vector-op - lhs rhs))


(define (vector-mul lhs rhs)
  (vector-op * lhs rhs))


(define (vector-div lhs rhs)
  (vector-op / lhs rhs))


(define (vector-mad vec-a vec-b vec-c)
  (for/vector #:length (vector-length vec-a) ([a vec-a][b vec-b][c vec-c])
    (+ (* a b) c)))


(define (vector-min lhs rhs)
  (vector-op min lhs rhs))


(define (vector-max lhs rhs)
  (vector-op max lhs rhs))


; Apply a function to each lane in the vector
(define (vector-map fn vec)
  (for/vector #:length (vector-length vec) ([lane vec])
    (fn lane)))


; Dot product of two vectors
(define (dot lhs rhs)
  (for/fold ([sum 0]) ([a lhs] [b rhs])
    (+ sum (* a b))))


; Distance between two vectors
(define (distance lhs rhs)
  (sqrt
   (for/fold ([sum 0]) ([a lhs] [b rhs])
     (+ sum (expt (- b a) 2)))))


; Geometric length of a vector
(define (vector-mag vec)
  (sqrt (dot vec vec)))


; Scalar lerp
(define (lerp lhs rhs alpha)
  (+
   (* lhs (- 1.0 alpha))
   (* rhs alpha)))


; Vector lerp
(define (vector-lerp lhs rhs alpha)
  (vector-op
   (lambda (lhs rhs) (lerp lhs rhs alpha))
   lhs rhs))


; Rotation functions
(define (rotate-2D point radians)
  (define sin-r (sin radians))
  (define cos-r (cos radians))
  (define x (vector-ref point 0))
  (define y (vector-ref point 1))
  (values (+ (* cos-r x) (* sin-r y))
          (- (* cos-r y) (* sin-r x))))


(define (vector-rotate vec axis radians)
  (cond [(eq? axis 'x)
         (define-values (y z) (rotate-2D (swiz vec 1 2) radians))
         (vector (vector-ref vec 0) y z)]
        [(eq? axis 'y)
         (define-values (x z) (rotate-2D (swiz vec 0 2) radians))
         (vector x (vector-ref vec 1) z)]
        [(eq? axis 'z)
         (define-values (x y) (rotate-2D (swiz vec 0 1) radians))
         (vector x y (vector-ref vec 2))]))
