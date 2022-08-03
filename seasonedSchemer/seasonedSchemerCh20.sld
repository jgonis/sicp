(define-library (seasoned-schemer ch20)
  (export jeff-ch20)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define jeff-ch20
      (lambda (x)
        (* x 2)))))
