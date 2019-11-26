(include "seasonedSchemerCh13.sld")
(define-library (seasoned-schemer tests ch13)
  (export run-tests-ch13)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch13))
  (begin
    (define run-tests-ch13
      (lambda ()
        (test-ch13)))
    (define test-ch13
      (lambda ()
        (check (ch13 2) => 4)))))
