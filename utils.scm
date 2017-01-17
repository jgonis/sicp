(define-library (sicp math-funcs)
  (export PI
          cube
          average)
  (import (scheme base))
  (begin
    (define PI 3.141592653589793238462643383279)
    (define (cube x)
      (* x x x))
     (define (average x y)
      (/ (+ x y) 2))))

(define-library (sicp ordered-list-set-lib)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set)
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
            (else (helper x set (car less-than)))))))


(define-library (sicp tree-lib)
  (export node
          left-branch
          right-branch
          make-tree
          tree-height
          tree->list
          list->balanced-tree)
  (import (scheme base))
  (begin
    (define (node tree)
      (car tree))

    (define (left-branch tree)
      (car (cdr tree)))

    (define (right-branch tree)
      (car (cdr (cdr tree))))

    (define (make-tree node left right)
      (list node left right))

    (define (tree-height tree)
      (cond ((null? tree) 0)
            (else (+ 1 (max (tree-height (left-branch tree))
                            (tree-height (right-branch tree)))))))

    (define (tree->list tree)
      (define (copy-to-list tree result-list)
        (cond ((null? tree)
               result-list)
              (else
               (copy-to-list
                (left-branch tree)
                (cons (node tree)
                      (copy-to-list
                       (right-branch tree)
                       result-list))))))
      (copy-to-list tree '()))
    
    (define (list->balanced-tree lyst)
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
