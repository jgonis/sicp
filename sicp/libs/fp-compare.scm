(define-library (libs fp-compare)
  (export fp-eq?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write))
  (begin
    (define fp-eq?
      (case-lambda
        ((x y) (fp-eq? x y 0.0000001))
        ((x y tolerance)
         (< (abs (- x y))
            tolerance))))))
