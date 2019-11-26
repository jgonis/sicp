(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch13)
  (export ch13)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define ch13
      (lambda (x)
        (* x 2)))))

