(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch12)
  (export ch12)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define ch12
      (lambda (x)
        (* x 2)))))

