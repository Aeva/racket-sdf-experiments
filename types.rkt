#lang racket

(require "math.rkt")

(provide sample
         sample-unpack
         field
         field-extent
         field-center
         field-min
         field-max
         field-combine
         colorize
         scale
         union
         cut
         sphere)


; Value to be returned by distance functions.
(struct sample (dist color)
  #:transparent)


(define (sample-unpack sample)
  (values (sample-dist sample)
          (sample-color sample)))


; Distance function base type.
; All operations, shapes, transforms, and so on should derive from this struct.
(struct field (center extent)
  #:transparent
  #:property prop:procedure
  (λ (self point)
    (sample +inf.f "black")))


; Negative-most corner of a given field.
(define (field-min field)
  (vector-op - (field-center field) (field-extent field)))


; Positive-most corner of a given field.
(define (field-max field)
  (vector-op + (field-center field) (field-extent field)))


; Combine the extents of two fields, and generate a new center.
(define (field-combine lhs rhs)
  (define min-corner
    (vector-op min (field-min lhs) (field-min rhs)))
  (define max-corner
    (vector-op max (field-max lhs) (field-max rhs)))
  (define extent
    (for/vector ([min-lane min-corner]
                 [max-lane max-corner])
      (/ (- max-lane min-lane) 2)))
  (define center
    (vector-op + min-corner extent))
  (values center extent))


; Generic field indirection type for making transforms with.
(struct indirection-field (proc)
  #:super struct:field
  #:transparent
  #:property prop:procedure
  (λ (self point)
    ((indirection-field-proc self) point)))


; Colorize a distance field
(define (colorize wrapped color)
  (define center (field-center wrapped))
  (define extent (field-extent wrapped))
  (define (proc point)
    (define dist (sample-dist (wrapped point)))
    (sample dist color))
  (indirection-field center extent proc))


; Scale field transform constructor.
(define (scale wrapped amount)
  (define center (field-center wrapped))
  (define extent
    (for/vector ([lane (field-extent wrapped)])
      (* lane amount)))
  (define factor (/ 1 amount))
  
  (define (proc point)
    (define scaled
      (for/vector ([lane point])
        (* lane factor)))
    (define-values
      (dist color)
      (sample-unpack (wrapped scaled)))
    (sample (* dist amount) color))
  (indirection-field center extent proc))


; Distance field union operator.
(define (union lhs rhs . more)
  (define-values (center extent)
    (field-combine lhs rhs))
  
  (define (proc point)
    (define lhs-sample (lhs point))
    (define rhs-sample (rhs point))
    (if ((sample-dist lhs-sample) . <= . (sample-dist rhs-sample))
        lhs-sample
        rhs-sample))
  (define combined (indirection-field center extent proc))

  (if (eq? more null)
      combined
      (apply union (cons combined more))))


; Distance field cut operator.
(define (cut lhs rhs)
  (define-values (center extent)
    (field-combine lhs rhs))
  
  (define (proc point)
    (define-values (lhs-dist lhs-color) (sample-unpack (lhs point)))
    (define rhs-dist (sample-dist (rhs point)))
    (define dist (max lhs-dist (* -1 rhs-dist)))
    (sample dist lhs-color))
  (indirection-field center extent proc))


; Sphere distance function type and constructor.
(struct sphere-field (radius)
  #:super struct:field
  #:transparent
  #:property prop:procedure
  (λ (self point)
    (sample 
     (- (distance point (field-center self)) (sphere-field-radius self))
     "black")))

(define (sphere center radius)
  (define extent
    (for/vector ([lane center])
      (+ lane radius)))
  (sphere-field center extent radius))
