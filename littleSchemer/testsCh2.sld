(include "littleSchemerCh2.sld")
(define-library (little-schemer tests ch2)
  (export run-tests-ch2)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (little-schemer ch2))
  (begin
    (define run-tests-ch2
      (lambda ()
        (test-lat?)
        (test-member?)))
        (define test-lat?
      (lambda ()
        (check (lat? '(Jack Sprat could eat no chicken fat)) => #t)
        (check (lat? '((Jack) Sprat could eat no chicken fat)) => #f)
        (check (lat? '(Jack (Sprat could) eat no chicken fat)) => #f)
        (check (lat? '()) => #t)))
    (define test-member?
      (lambda ()
        (check (member? 'tea
                        '(coffee tea or milk))
               => #t)
        (check (member? 'poached
                       '(fried eggs and scrambled eggs))
               => #f)
        (check (member? 'meat
                        '(mashed potatoes and meat gravy))
               => #t)))))
