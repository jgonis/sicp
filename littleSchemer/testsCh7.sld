(include "littleSchemerCh7.sld")
(define-library (little-schemer tests ch7)
  (export run-tests-ch7)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch7))
  (begin
    (define run-tests-ch7
      (lambda ()
        (test-set?)))
    (define test-set?
      (lambda ()
        (check (set? '(apples peaches pears plums))
               => #t)))))
