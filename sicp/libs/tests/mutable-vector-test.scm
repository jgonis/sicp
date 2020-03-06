(define-library (libs tests mutable-vector-test)
  (export run-tests)
  (import (scheme base)
          (srfi 64)
          (libs mutable-vector))
  (begin
    (define (run-tests arg))))
