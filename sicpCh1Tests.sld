(include "sicpCh1.sld")
(define-library (sicp tests ch1)
  (export run-tests-ch1)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (sicp ch1))
  (begin
    (define run-tests-ch1
      (lambda ()
        (test-placeholder)))
    (define test-placeholder
      (lambda ()
        (check (placeholder 1) => 1)))))
