(include "testsCh1.sld")
(include "testsCh2.sld")
(include "testsCh3.sld")
(include "testsCh4.sld")
(include "testsCh5.sld")
(include "testsCh6.sld")
(include "testsCh7.sld")
(include "testsCh8.sld")
(include "testsCh9.sld")
(define-library (little-schemer tests all)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer tests ch1)
          (little-schemer tests ch2)
          (little-schemer tests ch3)
          (little-schemer tests ch4)
          (little-schemer tests ch5)
          (little-schemer tests ch6)
          (little-schemer tests ch7)
          (little-schemer tests ch8)
          (little-schemer tests ch9))
  (begin
    (define run-tests
      (lambda ()
        (check-reset!)
        (check-set-mode! 'report-failed)
        (run-tests-ch1)
        (run-tests-ch2)
        (run-tests-ch3)
        (run-tests-ch4)
        (run-tests-ch5)
        (run-tests-ch6)
        (run-tests-ch7)
        (run-tests-ch8)
        (run-tests-ch9)
        (check-report)
        (check-reset!)))))
