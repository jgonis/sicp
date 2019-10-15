(include "littleSchemerCh1.sld")
(define-library (little-schemer ch8)
  (export test-func)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define test-func
      (lambda ()
        #t))))



