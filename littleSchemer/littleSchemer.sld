(define-library (little-schemer)
  (export test-func)
  (import (scheme base)
          (scheme write))
  (begin
    (define test-func
      (lambda (x)
        (* x 4)))))
