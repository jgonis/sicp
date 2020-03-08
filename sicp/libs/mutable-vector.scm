(define-library (libs mutable-vector)
  (export make-mutable-vector
          mutable-vector?
          mutable-vector-length
          add-element
          mutable-vector-ref
          get-buff)
  (import (scheme base))
  (begin
    (define-record-type mutable-vector
      (make-mutable-vec used buff capacity)
      mutable-vector?
      (used get-used set-used!)
      (buff get-buff))
    
    (define (make-mutable-vector . initial-capacity)      
      (cond ((null? initial-capacity)
             (make-mutable-vec 0 (make-vector 1)))
            (else (let ((initial-capacity (car initial-capacity)))
                    (if (argument-is-positive-integer? initial-capacity)
                        (make-mutable-vec 0 (make-vector initial-capacity))
                        (error "invalid argument for initial vector capacity" initial-capacity))))))
    
    (define (mutable-vector-length mutable-vec)
      (get-used mutable-vec))

    (define (add-element mutable-vec element)
      (let ((used (get-used mutable-vec)))
        (set-used! mutable-vec (+ used 1))
        (vector-set! (get-buff mutable-vec) used element)))

    (define (mutable-vector-ref mutable-vec idx)
      (vector-ref (get-buff mutable-vec) idx))

    (define (argument-is-positive-integer? arg)
      (and (integer? arg)
           (> arg 0)))))
