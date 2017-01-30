(include "Multisets.sld")
(define-library (sicp test-multiset)
  (export test-create-multiset
          test-union
          test-intersection
          test-element?
          test-element-count
          test-adjoin
          test-remove
          test-all)
  (import (scheme base)
          (scheme write)
          (srfi 78)
          (sicp multi-sets))
  (begin
    (define (test-create-multiset) #f)
    (define (test-union) #f)
    (define (test-intersection) #f)
    (define (test-element?)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '())))
        (check (element-of-set? 1 test-subject) => #t)
        (check (element-of-set? 2 test-subject) => #f))
      (let* ((test-subject (adjoin-set 3
                                       (adjoin-set 1
                                                   (adjoin-set 2 '())))))
        (check (element-of-set? 1 test-subject) => #t)
        (check (element-of-set? 2 test-subject) => #t)
        (check (element-of-set? 3 test-subject) => #t))
      (check-report)
      (check-reset!)
      (display ""))
    (define (test-element-count)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '()))
             (test-subject2 (adjoin-set 1 test-subject)))
        (check (element-count 1 test-subject) => 1)
        (check (element-count 1 test-subject2) => 2))
      (check-report)
      (check-reset!)
      (display ""))
    (define (test-adjoin)
      (check-set-mode! 'summary)
      (let* ((test-subject (adjoin-set 1 '())))
        (check (size-set test-subject) => 1))
      (check-report)
      (check-reset!)
      (display ""))
    (define (test-remove)
      (check-set-mode! 'summary)
      (let* ((test-obj (adjoin-set 2 (adjoin-set 1 '())))
             (test-subject (remove-set 1 test-obj)))
        1)
      (check-report)
      (check-reset!)
      (display ""))
    (define (test-all)
      (test-union)
      (test-intersection)
      (test-element?)
      (test-element-count)
      (test-adjoin)
      (test-remove))))
