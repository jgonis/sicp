(include "tree.sld")
(define-library (sicp multi-sets)
  (export union-set
          intersection-set
          element-of-set?
          element-count
          adjoin-set
          remove-set
          size-set)
  (import (scheme base))
  (begin
    (define (union-set set1 set2) '())
    (define (intersection-set set1 set2) '())
    (define (element-of-set? x set) #f)
    (define (element-count x set) 0)
    (define (adjoin-set x set) '())
    (define (remove-set x set) '())))
