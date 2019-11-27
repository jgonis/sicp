(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch14)
  (export ch14)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define ch14
      (lambda (x)
        (* x 2)))))
