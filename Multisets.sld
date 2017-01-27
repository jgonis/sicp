(include "tree.sld")
(define-library (sicp multi-sets)
  (export union-set
          intersection-set
          element-of-set?
          element-count
          adjoin-set
          remove-set
          size-set)
  (import (scheme base)
          (sicp tree-lib))
  (begin
    (define (union-set set1 set2))
    (define (intersection-set set1 set2) '())
    (define (element-of-set? x set) #f)
    (define (element-count x set) 0)
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree (cons x 1) '() '))))
    (define (size-set set)
      (length (tree->list)))
    (define (remove-set x set) '())))
