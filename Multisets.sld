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
    (define (union-set set1 set2) '())
    (define (intersection-set set1 set2) '())
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((= x (car (node set))) #t)
            ((< x (car (node set)))
             (element-of-set? x (left-branch set)))
            ((> x (car (node set)))
             (element-of-set? x (right-branch set)))))
    
    (define (element-count x set)
      (cond ((null? set) (error "element was not found!" x))
            ((= x (car (node set)))
             (cdr (node set)))
            ((< x (car (node set)))
             (element-count x (left-branch set)))
            ((> x (car (node set)))
             (element-count x (right-branch set)))))
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree (cons x 1) '() '()))
            ((= x (car (node set)))
             (make-tree (cons x (+ (cdr (node set)) 1))
                        (left-branch set)
                        (right-branch set)))
            ((< x (car (node set)))
             (make-tree (node set)
                        (adjoin-set x
                                    (left-branch set))
                        (right-branch set)))
            ((> x (car (node set)))
             (make-tree (node set)
                        (left-branch set)
                        (adjoin-set x
                                    (right-branch set))))))
    (define (size-set set)
      (length (tree->list set)))
    (define (remove-set x set)
      (define (helper x old-list new-list)
        (cond ((null? old-list) (reverse new-list))
              ((= x (car (car old-list))) (helper x
                                                  (cdr old-list)
                                                  new-list))
              (else (helper x
                            (cdr old-list)
                            (cons (car old-list)
                                  new-list)))))
      (helper x (tree->list set) '()))))
