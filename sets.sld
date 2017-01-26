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
          remove-set
          size-set)
  (import (scheme base)
          (sicp tree-lib)
          (prefix (sicp ordered-list-set) ol-))
  (begin
    (define (union-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (list->balanced-tree (ol-union-set (tree->list set1)
                                           (tree->list set2) comparator)))
      (cond ((null? less-than) (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))
    
    (define (intersection-set set1 set2 . less-than)
      (define (helper set1 set2 comparator)
        (list->balanced-tree (ol-intersection-set (tree->list set1)
                                                  (tree->list set2))))
      (cond ((null? less-than) (helper set1 set2 (lambda (a b) (< a b))))
            (else (helper set1 set2 (car less-than)))))
    
    (define (element-of-set? x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) #f)
              ((and (not (comparator x (node set)))
                    (not (comparator (node set) x))) #t)
              ((comparator x (node set)) (helper x
                                                 (left-branch set)
                                                 comparator))
              ((comparator (node set) x) (helper x
                                                 (right-branch set)
                                                 comparator))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (adjoin-set x set . less-than)
      (define (helper x set comparator)
        (cond ((null? set) (make-tree x '() '()))
              ((and (not (comparator x (node set)))
                    (not (comparator (node set) x))) set)
              ((comparator x (node set))
               (make-tree (node set)
                          (helper x
                                  (left-branch set)
                                  comparator)
                          (right-branch set)))
              ((comparator (node set) x)
               (make-tree (node set)
                          (left-branch set)
                          (helper x
                                  (right-branch set)
                                  comparator)))))
      (cond ((null? less-than) (helper x set (lambda (a b) (< a b))))
            (else (helper x set (car less-than)))))
    
    (define (remove-set x set . less-than)
      (define (remove-helper x set result comparator)
        (cond ((null? set) (reverse result))
              ((and (not (comparator x (car set)))
                    (not (comparator (car set) x)))
               (remove-helper x
                              (cdr set)
                              result
                              comparator))
              (else (remove-helper x
                                   (cdr set)
                                   (cons (car set) result)
                                   comparator))))
       (cond ((null? less-than) (list->balanced-tree
                                 (remove-helper x
                                                (tree->list set)
                                                '()
                                                (lambda (a b) (< a b)))))
             (else (list->balanced-tree (remove-helper x
                                                       (tree->list set)
                                                       '()
                                                       (car less-than))))))
    (define (size-set set)
      (length (tree->list set)))))


