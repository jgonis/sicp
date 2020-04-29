(define-library (libs mutable-vector-messages)
  (export INVALID_CAPACITY_ERROR
          INVALID_INDEX_ERROR)
  (import (scheme base))
  (begin
    (define INVALID_CAPACITY_ERROR "Invalid argument for initial vector capacity")
    (define INVALID_INDEX_ERROR "Invalid index for vector access")))
