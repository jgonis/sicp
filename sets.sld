(include "tree.sld")
(define-library (sicp ordered-list-set)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set
          remove-set
          size-set)
  (import (scheme base))
  (begin
    (define (union-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (cond ((null? set1) set2)
              ((null? set2) set1)
              (else
               (let ((x1 (car set1))
                     (x2 (car set2)))
                 (cond ((comparator x1 x2) (cons x1 (helper (cdr set1)
                                                            set2
                                                            comparator)))
                       ((comparator x2 x1) (cons x2 (helper set1
                                                            (cdr set2)
                                                            comparator)))
                       (else (cons x1 (helper (cdr set1)
                                              (cdr set2)
                                              comparator))))))))
      (cond ((null? less-than)
             (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))
    
    (define (intersection-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (cond ((or (null? set1) (null? set2)) '())
              (else
               (let ((x1 (car set1))
                     (x2 (car set2)))
                 (cond ((and (not (comparator x1 x2))
                             (not (comparator x2 x1)))
                        (cons x1 (helper
                                  (cdr set1)
                                  (cdr set2)
                                  comparator)))
                       ((comparator x1 x2) (helper (cdr set1)
                                                   set2
                                                   comparator))
                       ((comparator x2 x1) (helper set1
                                                   (cdr set2)
                                                   comparator)))))))
      (cond ((null? less-than) (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))
    
    (define (element-of-set? x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) #f)
              ((and (not (comparator x (car set)))
                    (not (comparator (car set) x))) #t)
              ((comparator x (car set)) #f)
              (else (helper x (cdr set) comparator))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (adjoin-set x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) (cons x '()))
              ((comparator x (car set)) (cons x set))
              ((and (not (comparator x (car set)))
                    (not (comparator (car set) x))) set)
              (else (cons (car set) (helper x (cdr set) comparator)))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (remove-set element set . less-than)
      (define (helper element set comparator)
        (cond ((null? set) set)
              ((and (not (comparator element (car set)))
                    (not (comparator (car set) element)))
               (helper element (cdr set) comparator))
              (else (cons (car set) (helper element
                                            (cdr set)
                                            comparator)))))
      (cond ((null? less-than) (helper element set (lambda (a b) (< a b))))
            (else (helper element set (car less-than)))))
    
    (define (size-set set) (length set))))

(define-library (sicp binary-tree-set)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set
          list->tree
          partial-tree)
  (import (scheme base)
          (sicp tree-lib)
          (prefix (sicp ordered-list-set) ol-))
  (begin
    (define (union-set set1 set2)
      (list->tree (ol-union-set (tree->list set1)
                                (tree->list set2))))
    (define (intersection-set set1 set2)
      (list->tree (ol-intersection-set (tree->list set1)
                                       (tree->list set2))))
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((= x (node set)) #t)
            ((< x (node set)) (element-of-set? x
                                               (left-branch set)))
            ((> x (node set)) (element-of-set? x
                                               (right-branch set)))))
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree x '() '()))
            ((= x (node set)) set)
            ((< x (node set))
             (make-tree (node set)
                        (adjoin-set x
                                    (left-branch set))
                        (right-branch set)))
            ((> x (node set))
             (make-tree (node set)
                        (left-branch set)
                        (adjoin-set x
                                    (right-branch set))))))
        
    (define (list->tree lyst)
      (car (partial-tree lyst
                         (length lyst))))

    (define (partial-tree elements n)
      (cond ((= n 0) (cons '() elements))
            (else (let* ((left-size (quotient (- n 1) 2))
                         (left-result (partial-tree elements
                                                    left-size))
                         (left-tree (car left-result))
                         (non-left-elements (cdr left-result))
                         (right-size (- n (+ left-size 1)))
                         (this-entry (car non-left-elements))
                         (right-result (partial-tree
                                        (cdr non-left-elements)
                                        right-size))
                         (right-tree (car right-result))
                         (remaining-elements (cdr right-result)))
                    (cons (make-tree this-entry
                                     left-tree
                                     right-tree)
                          remaining-elements)))))))
