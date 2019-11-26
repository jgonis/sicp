(include "seasonedSchemerCh1.sld")
(define-library (seasoned-schemer tests ch1)
  (export run-tests-ch1)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch1))
  (begin
    (define run-tests-ch1
      (lambda ()
        (test-two-in-a-row?)))
    (define test-two-in-a-row?
      (lambda ()
        (check (two-in-a-row? '(italian
                                sardines
                                spaghetti
                                parsley))
               => #f)))))
