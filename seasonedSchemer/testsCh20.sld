(define-library (seasoned-schemer tests ch20)
  (export run-tests-ch20)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch20))
  (begin
    (define run-tests-ch20
      (lambda ()
        (check-reset!)
        (check-set-mode! 'report-failed)
        (test-jeff-ch20)
        (check-report)
        (check-reset!)))
    (define  test-jeff-ch20
      (lambda ()
        (check (jeff-ch20 2)
               => 4)))))
