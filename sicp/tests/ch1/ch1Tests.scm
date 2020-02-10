(define-library (sicp tests ch1 ch1Tests)
  (export ch1-tests)
  (import (scheme base)
          (scheme write)
          (sicp ch1 ch1)
          (srfi 64))
  (begin
    (define ch1-tests
      (lambda ()
        (test-begin "ch1 tests")
        (test-eqv 326 (test-func 5))
        (test-end "ch1 tests")))))
