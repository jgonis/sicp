(include "sets.sld")
(define-library (sicp test-binary-tree-set)
  (export test-union
          test-intersection
          test-element-of-set?
          test-adjoin
          test-remove
          test-all)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (sicp binary-tree-set))
  (begin
     (define (test-union)
       (define (comparator a b)
         (display "test")
         (< a b))
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 1 '()))
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))
      
      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 '())
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 '())
             (test-subject2 (adjoin-set 1 '()))
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 '())
             (test-subject2 '())
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))      
      
      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 2 '()))
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 2)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #t))
      
      (let* ((test-subject1 (adjoin-set 4
                                        (adjoin-set 5
                                                    (adjoin-set 3 '()))))
             (test-subject2 (adjoin-set 1 (adjoin-set 2 '())))
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 5)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #t)
        (check (element-of-set? 3 result) => #t)
        (check (element-of-set? 4 result) => #t)
        (check (element-of-set? 5 result) => #t))
      
      (let* ((test-subject1 (adjoin-set 3
                                        (adjoin-set 2
                                                    (adjoin-set 1 '()))))
             (test-subject2 (adjoin-set 5
                                        (adjoin-set 4
                                                    (adjoin-set 2 '()))))
             (result (union-set test-subject1 test-subject2)))
        (check (size-set result) => 5)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #t)
        (check (element-of-set? 3 result) => #t)
        (check (element-of-set? 4 result) => #t)
        (check (element-of-set? 5 result) => #t))      
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-intersection)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 2 '()))
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 0))

      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 2 (adjoin-set 1 '())))
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 (adjoin-set 2 (adjoin-set 1 '())))
             (test-subject2 (adjoin-set 1 '()))
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 '())
             (test-subject2 '())
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))

      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 '())
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))
      
      (let* ((test-subject1 '())
             (test-subject2 (adjoin-set 1 '()))
             (result (intersection-set test-subject1 test-subject2)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-element-of-set?)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '())))
        (check (element-of-set? 1 test-subject) => #t)
        (check (element-of-set? 2 test-subject) => #f)
        (check (element-of-set? -1 test-subject) => #f))
      (let* ((test-subject '()))
        (check (element-of-set? 1 test-subject) => #f)
        (check (element-of-set? 2 test-subject) => #f))
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-adjoin)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 2 test-subject1))
             (test-subject3 (adjoin-set -1 test-subject1)))
        (check (size-set test-subject1) => 1)
        (check (size-set test-subject2) => 2)
        (check (element-of-set? 1 test-subject1) => #t)
        (check (element-of-set? 2 test-subject1) => #f)
        (check (element-of-set? 2 test-subject2) => #t)
        (check (element-of-set? -1 test-subject2) => #f)
        (check (element-of-set? -1 test-subject3) => #t))
      (check-report)
      (check-reset!)
      (display ""))

    (define (test-remove)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '()))
             (result (remove-set 0 test-subject1)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t))
      
      (let* ((test-subject1 (adjoin-set 1 '()))
             (result (remove-set 1 test-subject1)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))
      
      (let* ((test-subject1 (adjoin-set 2 (adjoin-set 1 '())))
             (result (remove-set 1 test-subject1)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #f)
        (check (element-of-set? 2 result) => #t)
        (check-report)
        (check-reset!)
        (display "")))
      
    (define (test-all)
      (test-union)
      (test-intersection)
      (test-element-of-set?)
      (test-adjoin)
      (test-remove))))
  
(define-library (sicp test-ordered-set)
  (export test-union
          test-intersection
          test-element-of-set?
          test-adjoin
          test-all)
  (import (scheme base)
          (srfi 78)
          (scheme write)
          (sicp ordered-list-set))
  (begin
    (define (comparator a b)
      (< a b))
    (define (test-union)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 (adjoin-set 1 '() comparator))
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #f))
      
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 '())
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #f))

      (let* ((test-subject1 '())
             (test-subject2 (adjoin-set 1 '() comparator))
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #f))
                                        
      (let* ((test-subject1 '())
             (test-subject2 '())
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result comparator) => #f))      
      
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 (adjoin-set 2 '() comparator))
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 2)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #t))
      
      (let* ((test-subject1 (adjoin-set
                             4
                             (adjoin-set
                              5
                              (adjoin-set
                               3
                               '()
                               comparator)
                              comparator)
                             comparator))
             (test-subject2 (adjoin-set 1
                                        (adjoin-set 2
                                                    '()
                                                    comparator)
                                        comparator))
             (result (union-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 5)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #t)
        (check (element-of-set? 3 result comparator) => #t)
        (check (element-of-set? 4 result comparator) => #t)
        (check (element-of-set? 5 result comparator) => #t))
      
      (let* ((test-subject1 (adjoin-set
                             3
                             (adjoin-set
                              2
                              (adjoin-set
                               1
                               '()
                               comparator)
                              comparator)
                             comparator))
             (test-subject2 (adjoin-set
                             5
                             (adjoin-set
                              4
                              (adjoin-set 2
                                          '()
                                           comparator)
                               comparator)
                              comparator))
             (result (union-set test-subject1 test-subject2  comparator)))
        (check (size-set result) => 5)
        (check (element-of-set? 1 result comparator) => #t)
        (check (element-of-set? 2 result comparator) => #t)
        (check (element-of-set? 3 result comparator) => #t)
        (check (element-of-set? 4 result comparator) => #t)
        (check (element-of-set? 5 result comparator) => #t))      
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-intersection)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 (adjoin-set 2 '() comparator))
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 0))

      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 (adjoin-set 2 (adjoin-set 1 '() comparator) comparator))
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 (adjoin-set 2 (adjoin-set 1 '() comparator) comparator))
             (test-subject2 (adjoin-set 1 '() comparator))
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result) => #t)
        (check (element-of-set? 2 result) => #f))

      (let* ((test-subject1 '())
             (test-subject2 '())
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))

      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 '())
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))
      
      (let* ((test-subject1 '())
             (test-subject2 (adjoin-set 1 '() comparator))
             (result (intersection-set test-subject1 test-subject2 comparator)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result) => #f))
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-element-of-set?)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '() comparator)))
        (check (element-of-set? 1 test-subject comparator) => #t)
        (check (element-of-set? 2 test-subject comparator) => #f)
        (check (element-of-set? -1 test-subject comparator) => #f))
      (let* ((test-subject '()))
        (check (element-of-set? 1 test-subject comparator) => #f)
        (check (element-of-set? 2 test-subject comparator) => #f))
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-adjoin)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (test-subject2 (adjoin-set 2 test-subject1 comparator))
             (test-subject3 (adjoin-set -1 test-subject1 comparator)))
        (check (size-set test-subject1) => 1)
        (check (size-set test-subject2) => 2)
        (check (element-of-set? 1 test-subject1 comparator) => #t)
        (check (element-of-set? 2 test-subject1 comparator) => #f)
        (check (element-of-set? 2 test-subject2 comparator) => #t)
        (check (element-of-set? -1 test-subject2 comparator) => #f)
        (check (element-of-set? -1 test-subject3 comparator) => #t))
      (check-report)
      (check-reset!)
      (display ""))

    (define (test-remove)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (result (remove-set 0 test-subject1 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result comparator) => #t))
      
      (let* ((test-subject1 (adjoin-set 1 '() comparator))
             (result (remove-set 1 test-subject1 comparator)))
        (check (size-set result) => 0)
        (check (element-of-set? 1 result comparator) => #f))
      
      (let* ((test-subject1 (adjoin-set 2 (adjoin-set 1 '() comparator) comparator))
             (result (remove-set 1 test-subject1 comparator)))
        (check (size-set result) => 1)
        (check (element-of-set? 1 result comparator) => #f)
        (check (element-of-set? 2 result comparator) => #t))
      (check-report)
      (check-reset!)
      (display ""))
    
    (define (test-all)
      (test-union)
      (test-intersection)
      (test-element-of-set?)
      (test-adjoin)
      (test-remove))))
