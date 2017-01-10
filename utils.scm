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

    (define (tree-height tree) 0)

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

