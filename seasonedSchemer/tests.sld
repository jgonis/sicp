(include "testsCh11.sld")
(include "testsCh12.sld")
(include "testsCh13.sld")
(include "testsCh14.sld")
(define-library (seasoned-schemer tests all)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer tests ch11)
          (seasoned-schemer tests ch12)
          (seasoned-schemer tests ch13)
          (seasoned-schemer tests ch14)
          (seasoned-schemer tests ch16))
  (begin
    (define run-tests
      (lambda ()
        (check-reset!)
        (check-set-mode! 'report-failed)
        (run-tests-ch11)
        (run-tests-ch12)
        (run-tests-ch13)
        (run-tests-ch14)
        (run-tests-ch16)
        (check-report)
        (check-reset!)))))
