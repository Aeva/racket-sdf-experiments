#lang racket

(provide swiz
         vector-op
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
