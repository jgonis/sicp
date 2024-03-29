(define-library (seasoned-schemer tests ch10)
  (export run-tests-ch10)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (seasoned-schemer ch10))
  (begin
    (define run-tests-ch10
      (lambda ()
        (check-reset!)
        (check-set-mode! 'report-failed)
        (test-lookup-in-entry)
        (test-lookup-in-table)
        (test-expression-to-type)
        (test-eval-cond-helpers)
        (test-cond)
        (test-primitive-application)
        (test-combine-tables)
        (check-report)
        (check-reset!)))
    (define test-lookup-in-entry
      (lambda ()
        (let ((test-val (new-entry '(appetizer entree beverage)
                                   '(pate beef wine))))
          (check (lookup-in-entry 'appetizer test-val (lambda (val) 'not-found))
                 => 'pate)
          (check (lookup-in-entry 'appetizer '() (lambda (val) 'not-found))
                 => 'not-found)
          (check (lookup-in-entry 'invalid-name test-val (lambda (val) 'not-found))
                 => 'not-found))))
    (define test-lookup-in-table
      (lambda ()
        (let* ((error-val 'name-not-found-in-table)
               (empty-table '())
               (test-entry-1 (new-entry '(entree desert) '(spaghetti spumoni)))
               (test-entry-2 (new-entry '(appetizer entree beverage) '(food tastes good)))
               (test-table (extend-table test-entry-1 (extend-table test-entry-2 empty-table))))
          (check (lookup-in-table 'entree empty-table (lambda (name) error-val))
                 => error-val)
          (check (lookup-in-table 'entree test-table (lambda (name) error-val))
                 => 'spaghetti)
          (check (lookup-in-table 'invalid-name test-table (lambda (name) error-val))
                 => error-val))))
    (define test-expression-to-type
      (lambda ()
        (check (expression-to-type 6 0)
               => '*const)
        (check (expression-to-type #f 0)
               => '*const)
        (check (expression-to-type 'cons 0)
               => '*const)
        (check (expression-to-type '(quote nothing) 0)
               => '*quote)
        (check (expression-to-type '(lambda (x y) (cons x y)) 0)
               => '*lambda)
        (check (expression-to-type 'jeff-var 0)
               => '*identifier)
        (check (expression-to-type '((lambda (nothing) (cond (nothing (quote something))
                                                        (else (quote nothing))))
                                     #t) 0)
               => '*application)
        (check (expression-to-type '(cond (nothing (quote something))
                                          (else (quote nothing))) 0)
               => '*cond)
        (check (expression-to-type '(jeff-func 10 20 30) 0)
               => '*application)))
    (define test-eval-cond-helpers
      (lambda ()
        (let* ((question-clause '(= 1 1))
               (else-primitive 'else)
               (answer-clause-1 '(display "hello world"))
               (answer-clause-2 '(display "end of cond"))
               (non-else-clause  (list question-clause answer-clause-1))
               (else-clause (list else-primitive answer-clause-2)))
          (check (question-of non-else-clause)
                 => question-clause)
          (check (question-of else-clause)
                 => else-primitive)
          (check (answer-of non-else-clause)
                 => answer-clause-1)
          (check (answer-of else-clause)
                 => answer-clause-2)
          (check (else? (question-of else-clause))
                 => #t))))
    (define test-cond
      (lambda ()
        (let* ((basic-else '(cond (else 2)))
               (nested-cond '(else (cond (else 2)))))
          (check (value basic-else)
                 => 2)
          (check (value '(cond (else (cond (else 2)))))
                 => 2)
          (check (meaning '(cond (coffee klatsch)
                                 (else party))
                          '(((coffee) (#t))
                            ((klatsch party) (5 (6)))) 0)
                 
                 => 5))))
    (define test-primitive-application
      (lambda ()
        (check (meaning '(cons 1 2) empty-table 0) 
               => '(1 . 2))
        (check (meaning '(+ 1 1) empty-table 0)
               => 2)
         (check (meaning '(zero? (- 1 1)) empty-table 0)
                => #t)
         ))

    (define test-combine-tables
      (lambda ()
        (let* ((first-table (extend-table (new-entry '(jeff) '(1)) empty-table))
               (second-table (extend-table (new-entry '(angie) '(2)) empty-table)))
          (check (combine-tables first-table second-table)
                 => '(((jeff) (1)) ((angie) (2)))))))
    ))
