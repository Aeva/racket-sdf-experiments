#lang racket

(provide swiz
         vector-op
         vector-add
         vector-sub
         vector-mul
         vector-div
         vector-mad
         dot
         distance
         vector-mag)


; Swizzle a vector
(define (swiz vec . lanes)
  (for/vector ([lane lanes])
    (vector-ref vec lane)))


; Lane-wise vector operators
(define (vector-op op lhs rhs)
  (for/vector ([a lhs][b rhs])
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
  (for/vector ([a vec-a][b vec-b][c vec-c])
    (+ (* a b) c)))


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
