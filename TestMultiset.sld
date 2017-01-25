(include "Multisets.sld")
(define-library (sicp test-multiset)
  (export test-union
          test-intersection
          test-element?
          test-element-count
          test-adjoin
          test-remove
          test-all)
  (import (scheme base)
          (scheme write)
          (srfi 78))
  (begin
    (define (test-union) #f)
    (define (test-intersection) #f)
    (define (test-element?) #f)
    (define (test-element-count) #f)
    (define (test-adjoin) #f)
    (define (test-remove) #f)
    (define (test-all)
      (test-union)
      (test-intersection)
      (test-element?)
      (test-element-count)
      (test-adjoin)
      (test-remove))))
