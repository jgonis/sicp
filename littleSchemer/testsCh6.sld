(include "littleSchemerCh6.sld")
(define-library (little-schemer tests ch6)
  (export run-tests-ch6)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch6))
  (begin
    (define run-tests-ch6
      (lambda ()
        (test-numbered?)
        (test-value)
        (test-prefix-value)
        (test-alt-value)))
    (define test-numbered?
      (lambda ()
        (check (numbered? 1)
               => #t)
        (check (numbered? '(3 + (4 ^ 5)))
               => #t)
        (check (numbered? '(2 * sausage))
               => #f)))
    (define test-value
      (lambda ()
        (check (value 13)
               => 13)
        (check (value '(1 + 3))
               => 4)
        (check (value '(1 + (3 ^ 4)))
               => 82)
        (check (value '((3 * 6) + (8 ^ 2)))
               => 82)))
    (define test-prefix-value
      (lambda ()
        (check (prefix-value '(+ 3 4))
               => 7)
        (check (prefix-value 3) => 3)
        (check (prefix-value '(+ (* 3 6) (^ 8 2)))
               => 82)))
    (define test-alt-value
      (lambda ()
        (check (alt-value '(+ 3 4))
               => 7)
        (check (alt-value 3) => 3)
        (check (alt-value '(+ (* 3 6) (^ 8 2)))
               => 82)))))
