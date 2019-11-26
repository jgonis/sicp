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
        (test-two-in-a-row?)
        (test-sum-of-prefixes)
        (test-scramble)))
    (define test-two-in-a-row?
      (lambda ()
        (check (two-in-a-row? '()) => #f)
        (check (two-in-a-row? '(a)) => #f)
        (check (two-in-a-row? '(italian
                                sardines
                                spaghetti
                                parsley))
               => #f)
        (check (two-in-a-row? '(italian
                                sardines
                                sardines
                                spaghetti
                                parsley))
               => #t)
        (check (two-in-a-row? '(italian
                                sardines
                                sardines))
               => #t)
        (check (two-in-a-row? '(b d e i i a g)) => #t)))
    (define test-sum-of-prefixes
      (lambda ()
        (check (sum-of-prefixes '(2 1 9 17 0))
               => '(2 3 12 29 29))
        (check (sum-of-prefixes '(1 1 1 1 1))
               => '(1 2 3 4 5))
        (check (sum-of-prefixes '()) => '())
        (check (sum-of-prefixes '(1)) => '(1))
        (check (sum-of-prefixes '(1 2)) => '(1 3))
        (check (sum-of-prefixes '(1 2 3)) => '(1 3 6))))
    (define test-scramble
      (lambda ()
        (check (scramble '(1 1 1 3 4 2 1 1 9 2))
               => '(1 1 1 1 1 4 1 1 1 9))
        (check (scramble '(1 2 3 4 5 6 7 8 9))
               => '(1 1 1 1 1 1 1 1 1))
        (check (scramble '(1 2 3 1 2 3 4 1 8 2 10))
               => '(1 1 1 1 1 1 1 1 2 8 2))
        (check (scramble '()) => '())
        (check (scramble '(1)) => '(1))))))
