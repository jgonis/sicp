(include "littleSchemerCh8.sld")
(define-library (little-schemer tests ch8)
  (export run-tests-ch8)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch8))
  (begin
    (define run-tests-ch8
      (lambda ()
        (test-test-func)))
    (define test-test-func
      (lambda ()
        (check (test-func) => #f)))))
