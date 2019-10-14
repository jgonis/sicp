(include "littleSchemerCh1.sld")
(define-library (little-schemer ch6)
  (export numbered?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define numbered?
      (lambda (arith-exp)
        #f))))



