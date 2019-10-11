(define-library (little-schemer ch1)
  (export atom?)
  (import (scheme base))
  (begin
    (define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))))
