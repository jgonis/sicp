(define-library (seasoned-schemer ch15)
  (export blah)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define blah
      (lambda (x)
        (* x 2)))))
