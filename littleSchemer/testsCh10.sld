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
        (test-lookup-in-entry)
        (test-lookup-in-table)))
    (define test-lookup-in-entry
      (lambda ()
        (let ((food (new-entry '(appetizer entree beverages)
                               '(food tastes good))))
          (check (lookup-in-entry 'entree
                                  food
                                  (lambda (name) #f))
                 => 'tastes)
          (check (lookup-in-entry 'dessert
                                  food
                                  (lambda (name)
                                    'entry-not-found))
                 => 'entry-not-found))))
    (define test-lookup-in-table
      (lambda ()
        (let ((table (extend-table
                      (new-entry '(entree dessert)
                                 '(spaghetti spumoni))
                      (extend-table
                       (new-entry '(appetizer entree beverage)
                                  '(food tastes good))
                       '()))))
          (check (lookup-in-table 'entree
                                  table
                                  (lambda (name)
                                    'name-not-found))
                 => 'spaghetti)
          (check (lookup-in-table 'beverage
                                  table
                                  (lambda (name)
                                    'name-not-found))
                 => 'good)
          (check (lookup-in-table 'jeff
                                  table
                                  (lambda (name)
                                    'name-not-found))
                 => 'name-not-found))))))
