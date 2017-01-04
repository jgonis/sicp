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
          make-tree)
  (import (scheme base))
  (begin
    (define (node tree)
      (car tree))
    (define (left-branch tree)
      (car (cdr tree)))
    (define (right-branch tree)
      (car (cdr (cdr tree))))
    (define (make-tree node left right)
      (list node left right))))
