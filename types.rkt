#lang racket

(require "math.rkt")

(provide sample
         sample-dist
         sample-color
         sample-unpack
         field
         field-extent
         field-center
         field-min
         field-max
         aabb
         aabb-min
         aabb-max
         aabb-merge
         field-combine
         field->aabb
         aabb-width
         aabb-height
         aabb-depth
         aabb-center
         aabb-radius
         aabb-flatten
         aabb-split
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


; Axially Aligned Bounding Box.
(struct aabb (min max)
  #:transparent)


; Create an AABB from a field object.
(define (field->aabb field)
  (aabb (field-min field)
        (field-max field)))


; Functions for extracting AABB dimensions.
(define (aabb-width aabb)
  (distance
   (swiz (aabb-min aabb) 0)
   (swiz (aabb-max aabb) 0)))


(define (aabb-height aabb)
  (distance
   (swiz (aabb-min aabb) 1)
   (swiz (aabb-max aabb) 1)))


(define (aabb-depth aabb)
  (distance
   (swiz (aabb-min aabb) 2)
   (swiz (aabb-max aabb) 2)))


; AABB center point
(define (aabb-center aabb)
  (for/vector ([min-lane (aabb-min aabb)]
               [max-lane (aabb-max aabb)])
    (+ min-lane (/ (- max-lane min-lane) 2))))


; Distance from AABB center to corner.
(define (aabb-radius aabb)
  (distance (aabb-min aabb) (aabb-center aabb)))


; Zeros the Z axis from an AABB.
(define (aabb-flatten old)
  (define flat-min (aabb-min old))
  (define flat-max (aabb-max old))
  (vector-set! flat-min 2 0)
  (vector-set! flat-max 2 0)
  (aabb flat-min flat-max))


; Split an AABB in half on one axis
(define (aabb-split old axis)
  (define old-min (aabb-min old))
  (define old-max (aabb-max old))
  (define center (aabb-center old))
  (define splice (vector 0 0 0))
  (vector-set! splice axis (exact-round (vector-ref center axis)))
  (define mask (vector 1 1 1))
  (vector-set! mask axis 0)
  (define new-min (vector-mad old-min mask splice))
  (define new-max (vector-mad old-max mask splice))
  (list (aabb old-min new-max) (aabb new-min old-max)))


; Merge two AABBs.
(define (aabb-merge lhs rhs)
  (aabb
   (vector-op min (aabb-min lhs) (aabb-min rhs))
   (vector-op max (aabb-max lhs) (aabb-max rhs))))


; Combine the extents of two fields, and generate a new center.
(define (field-combine lhs rhs)
  (define merged (aabb-merge (field->aabb lhs) (field->aabb rhs)))
  (define (realign span) (* span 0.5))
  (define extent
    (vector
     (realign (aabb-width merged))
     (realign (aabb-height merged))
     (realign (aabb-depth merged))))
  (define center
    (vector-op + (aabb-min merged) extent))
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
  (define center
    (for/vector ([lane (field-center wrapped)])
      (* lane amount)))
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
  (define extent (vector radius radius radius))
  (sphere-field center extent radius))
