(include "testsCh1.sld")
(include "testsCh2.sld")
(include "testsCh3.sld")
(include "testsCh4.sld")
(include "testsCh5.sld")
(define-library (little-schemer tests all)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer tests ch1)
          (little-schemer tests ch2)
          (little-schemer tests ch3)
          (little-schemer tests ch4)
          (little-schemer tests ch5))
  (begin
    (define run-tests
      (lambda ()
        (check-reset!)
        (run-tests-ch1)
        (run-tests-ch2)
        (run-tests-ch3)
        (run-tests-ch4)
        (run-tests-ch5)
        (check-report)
        (check-reset!)))))
