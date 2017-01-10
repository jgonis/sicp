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
          tree-list
          list-balanced-tree)
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
               (copy-to-list (left-branch tree)
                             (cons (node tree)
                                   (copy-to-list (right-branch tree)
                                                 result-list))))))
      (copy-to-list tree '()))
    (define (list->balanced-tree lyst)
      (car (partial-tree lyst
                         (length lyst))))
    ;;Exercise 2.64
    ;;partial-tree works by recursively dividing the list "in half",
    ;;and then calling partial-tree again once for the left tree and
    ;;once for the right tree. Once the list size we are sub-dividing
    ;;is down to one we create a tree with no children and return that
    ;;which in turn becomes the left or right branch of the parent node
    ;;and so on back up until we hit the (roughly) mid-point of the
    ;;list, which is our root node. Because we only visit each list
    ;;element once as we traverse through the list the runtime is O(n).
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
