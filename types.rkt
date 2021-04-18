#lang racket

(require "math.rkt")

(provide field
         field-extent
         field-center
         field-min
         field-max
         field-combine
         scale
         union
         sphere)


; Distance function base type.
; All operations, shapes, transforms, and so on should derive from this struct.
(struct field (center extent)
  #:transparent
  #:property prop:procedure
  (λ (self point)
    +inf.f))


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
    (wrapped scaled))
  (indirection-field center extent proc))


; Distance field union operator.
(define (union lhs rhs . more)
  (define-values (center extent)
    (field-combine lhs rhs))
  
  (define (proc point)
    (min (lhs point) (rhs point)))
  (define combined (indirection-field center extent proc))

  (if (eq? more null)
      combined
      (apply union (cons combined more))))


; Sphere distance function type and constructor.
(struct sphere-field (radius)
  #:super struct:field
  #:transparent
  #:property prop:procedure
  (λ (self point)
    (- (distance point (field-center self)) (sphere-field-radius self))))

(define (sphere center radius)
  (define extent
    (for/vector ([lane center])
      (+ lane radius)))
  (sphere-field center extent radius))
