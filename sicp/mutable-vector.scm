(define-library (sicp mutable-vector)
  (export make-mutable-vector
          add-element
          push-to-front
          remove-first)
  (import (scheme base))
  (begin
    (define-record-type mutable-vector
      (make-mutable-vec used buff size offset)
      mutable-vector?
      (used get-used set-used)
      (buff)
      size
      offset)

    (define make-mutable-vector ( . optional-size)
      (make-vector 1))

    (define add-element (mutable-vec element))

    (define push-to-front (mutable-vec element)
      mutable-vec)
    
    (define remove-first (mutable-vec) mutable-vec)

    (define remove-at (mutable-vec index) mutable-vec)

    (define insert-at (mutable-vec index element)
      mutable-vec)))
