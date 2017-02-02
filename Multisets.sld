(include "tree.sld")
(include "sets.sld")
(define-library (sicp multi-sets)
  (export create-multiset
          union-set
          intersection-set
          element-of-set?
          element-count
          adjoin-set
          remove-set
          size-set)
  (import (scheme base)
          (sicp tree-lib)
          (prefix (sicp ordered-list-set) ol-))
  (begin
    (define (create-multiset list-of-items)
      (define (helper list-of-items multi-set)
        (cond ((null? list-of-items)
               (list->balanced-tree (tree->list multi-set)))
              (else (helper (cdr list-of-items)
                            (adjoin-set (car list-of-items)
                                        multi-set)))))
      (helper list-of-items '()))

    (define (union-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (ol-union-set (tree->list set1) (tree->list set2) comparator))
      (cond ((null? less-than) (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))

    (define (intersection-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (ol-intersection-set (tree->list set1) (tree->list set2) comparator))
      (cond ((null? less-than) (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))

    (define (element-of-set? x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) #f)
              ((and (not (comparator x (car (node set))))
                    (not (comparator (car (node set)) x))) #t)
              ((comparator x (car (node set)))
               (helper x (left-branch set) comparator))
              ((comparator (car (node set)) x)
               (helper x (right-branch set) comparator))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (element-count x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) (error "element was not found!" x))
              ((and (not (comparator x (car (node set))))
                    (not (comparator (car (node set)) x)))
               (cdr (node set)))
              ((comparator x (car (node set)))
               (helper x (left-branch set) comparator))
              ((comparator (car (node set)) x)
               (helper x (right-branch set) comparator))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (adjoin-set x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) (make-tree (cons x 1) '() '()))
              ((and (not (comparator x (car (node set))))
                    (not (comparator (car (node set)) x)))
               (make-tree (cons x (+ (cdr (node set)) 1))
                          (left-branch set)
                          (right-branch set)))
              ((comparator x (car (node set)))
               (make-tree (node set)
                          (helper x
                                  (left-branch set)
                                  comparator)
                          (right-branch set)))
              ((comparator (car (node set)) x)
               (make-tree (node set)
                          (left-branch set)
                          (helper x
                                  (right-branch set)
                                  comparator)))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))

    (define (size-set set)
      (length (tree->list set)))

    (define (remove-set x set . less-than)
      (define (helper x old-list new-list comparator)
        (cond ((null? old-list) (reverse new-list))
              ((and (not (comparator x (car (car old-list))))
                    (not (comparator (car (car old-list)) x)))
               (helper x
                       (cdr old-list)
                       new-list
                       comparator))
              (else (helper x
                            (cdr old-list)
                            (cons (car old-list) new-list)
                            comparator))))
      (cond ((null? less-than)
             (list->balanced-tree (helper x
                                          (tree->list set)
                                          '()
                                          (lambda (a b) (< a b)))))
            (else (list->balanced-tree (helper x
                                               (tree->list set)
                                               '()
                                               (car less-than))))))))
