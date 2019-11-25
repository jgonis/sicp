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
        (test-lookup-in-table)
        (test-*const)
        (test-*quote)
        (test-*identifier)
        (test-*cond)
        (test-value)))
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
                 => 'name-not-found))))
    (define test-*const
      (lambda ()
        (let ((table '()))
          (check (*const #t table) => #t)
          (check (*const #f table) => #f)
          (check (*const 4 table) => 4)
          (check (*const 'car table) => '(primitive car))
          (check (*const 'cdr table) => '(primitive cdr))
          (check (*const 'null? table) => '(primitive null?))
          (check (*const 'eq? table) => '(primitive eq?))
          (check (*const 'atom? table) => '(primitive atom?))
          (check (*const 'zero? table) => '(primitive zero?))
          (check (*const 'add1 table) => '(primitive add1))
          (check (*const 'sub1 table) => '(primitive sub1))
          (check (*const 'number? table) => '(primitive number?)))))
    (define test-*quote
      (lambda ()
        (let ((table '()))
          (check (*quote '(quote (a b c)) table)
                 => '(a b c))
          (check (*quote '(quote 3) table) => 3))))
    (define test-*identifier
      (lambda ()
        (let ((table (extend-table (new-entry '(b)
                                              '(2))
                                   (extend-table (new-entry '(a)
                                                            '(1))
                                                 '()))))
          (check (*identifier 'a table) => 1)
          (check (*identifier 'b table) => 2))))
    (define test-*cond
      (lambda ()
        (let ((table (extend-table
                      (new-entry '(coffee) '(#t))
                      (extend-table
                       (new-entry '(klatsch party) '(5 (6)))
                       '()))))
          (check (*cond '(cond (coffee klatsch)
                               (else party))
                        table)
                 => 5))))
    (define test-value
      (lambda ()))))
