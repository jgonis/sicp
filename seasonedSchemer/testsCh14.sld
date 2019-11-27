(include "seasonedSchemerCh14.sld")
(define-library (seasoned-schemer tests ch14)
  (export run-tests-ch14)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch14))
  (begin
    (define run-tests-ch14
      (lambda ()
        (test-ch14)))
    (define test-ch14
      (lambda ()
        (check (ch14 2)
               => 4)))))
