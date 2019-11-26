(include "seasonedSchemerCh1.sld")
(define-library (seasoned-schemer tests ch1)
  (export run-tests-ch1)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch1))
  (begin
    (define run-tests-ch1
      (lambda ()
        (test-ch1test)))
    (define test-ch1test
      (lambda ()
        (check (ch1test 3) => 3)))))
