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
