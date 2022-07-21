(define-library (seasoned-schemer tests ch15)
  (export run-tests-ch15)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch15))
  (begin
    (define run-tests-ch15
      (lambda ()
        (check-reset!)
        (check-set-mode! 'report-failed)
        (test-blah)
        (check-report)
        (check-reset!)))
    (define test-blah
      (lambda ()
        (check (blah 2)
               => 4)))))
