(include "littleSchemerCh10.sld")
(define-library (little-schemer tests ch10)
  (export run-tests-ch10)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch10))
  (begin
    (define run-tests-ch10
      (lambda ()
        (test-lookup-in-entry)))
    (define test-lookup-in-entry
      (lambda ()
        (check (lookup-in-entry '() '() '())
               => #f)))))
