(include "littleSchemerCh9.sld")
(define-library (little-schemer tests ch9)
  (export run-tests-ch9)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch9))
  (begin
    (define run-tests-ch9
      (lambda ()
        (test-test-func)))
    (define test-test-func
      (lambda ()
        (check (test-func)
               => #f)))))
