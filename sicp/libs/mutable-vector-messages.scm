(define-library (libs mutable-vector-messages)
  (export INVALID_CAPACITY_ERROR)
  (import (scheme base))
  (begin
    (define INVALID_CAPACITY_ERROR "Invalid argument for initial vector capacity")))
