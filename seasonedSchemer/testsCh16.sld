(define-library (seasoned-schemer tests ch16)
  (export run-tests-ch16)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch16))
  (begin
    (define run-tests-ch16
      (lambda ()
        (test-deep)))
    (define test-deep
      (lambda ()
        (check (deep 0)
               => 'pizza)
        (check (deep 3)
               => '(((pizza))))
        (check (deep 7)
               => '(((((((pizza))))))))))))
