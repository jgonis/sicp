(include "littleSchemer.sld")
(define-library (little-schemer tests)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer))
  (begin
    (define run-tests
      (lambda ()
        (test1)
        (test2)
        (check-report)
        (check-reset!)))
    (define test1
      (lambda ()
        (check (test-func 2) => 8)))
    (define test2
      (lambda ()
        (check (> (test-func 2) 5) => #t)))))
