(include "Multisets.sld")
(define-library (sicp test-multiset)
  (export test-create-mutiset
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
    (define (test-element?) #f)
    (define (test-element-count) #f)
    (define (test-adjoin)
      (check-set-mode! 'summary)
      (let* ((test-subject1 (adjoin-set 1 '())))
        (check (size-set test-subject1) => 1))
      (check-report)
      (check-reset!)
      (display ""))
    (define (test-remove) #f)
    (define (test-all)
      (test-union)
      (test-intersection)
      (test-element?)
      (test-element-count)
      (test-adjoin)
      (test-remove))))
