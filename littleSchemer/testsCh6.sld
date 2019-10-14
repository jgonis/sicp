(include "littleSchemerCh6.sld")
(define-library (little-schemer tests ch6)
  (export run-tests-ch6)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch6))
  (begin
    (define run-tests-ch6
      (lambda ()
        (test-numbered?)))
    (define test-numbered?
      (lambda ()
        (check (numbered? '(1 + 3))
               => #f)))))
