(include "littleSchemerCh1.sld")
(define-library (little-schemer tests ch1)
  (export run-tests-ch1)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch1))
  (begin
    (define run-tests-ch1
      (lambda ()
        (test-atom?)))
    (define test-atom?
      (lambda ()
        (check (atom? (quote ())) => #f)))))
