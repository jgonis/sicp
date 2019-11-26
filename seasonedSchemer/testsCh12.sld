(include "seasonedSchemerCh12.sld")
(define-library (seasoned-schemer tests ch12)
  (export run-tests-ch12)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch12))
  (begin
    (define run-tests-ch12
      (lambda ()
        (test-ch12)))
    (define test-ch12
      (lambda ()
        (check (ch12 2) => 4)))))
