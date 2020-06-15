(define-library (libs mutable-vector)
  (export make-mutable-vector
          mutable-vector?
          mutable-vector-length
          add-element
          mutable-vector-ref
          get-buff)
  (import (scheme base)
          (libs mutable-vector-messages))
  (begin
    (define GROWTH_FACTOR 1.5)
    (define-record-type mutable-vector
      (make-mutable-vec used buff)
      mutable-vector?
      (used get-used set-used!)
      (buff get-buff set-buff!))
    
    (define (make-mutable-vector . initial-capacity)      
      (cond ((null? initial-capacity)
             (make-mutable-vec 0 (make-vector 1)))
            (else (let ((initial-capacity (car initial-capacity)))
                    (if (argument-is-positive-integer? initial-capacity)
                        (make-mutable-vec 0 (make-vector initial-capacity))
                        (error INVALID_CAPACITY_ERROR initial-capacity))))))
    
    (define (mutable-vector-length mutable-vec)
      (get-used mutable-vec))

    (define (add-element mutable-vec element)
      (let ((used (get-used mutable-vec))
            (capacity (get-capacity mutable-vec)))
        (if (= used capacity)
            (copy-old-buff-to-new-buff mutable-vec))
        (set-used! mutable-vec (+ used 1))
        (vector-set! (get-buff mutable-vec) used element)
        mutable-vec))
    
    (define (mutable-vector-ref mutable-vec idx)
      (if (or (< idx 0) (>= idx (get-used mutable-vec)))
          (error INVALID_INDEX_ERROR idx)
          (vector-ref (get-buff mutable-vec) idx)))

    (define (remove-element mutable-vec idx)
      (if (or (not (argument-is-positive-integer idx)) (>= idx (get-used mutable-vec)))
          (error INVALID_INDEX_ERROR idx)
          ()))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;Non-Exported Methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (get-capacity mutable-vec)
      (vector-length (get-buff mutable-vec)))
          
    (define (copy-old-buff-to-new-buff mutable-vec)
      (let* ((new-capacity (exact (ceiling (* (get-capacity mutable-vec) GROWTH_FACTOR))))
             (new-buff (make-vector new-capacity))
             (old-buff (get-buff mutable-vec)))
        (vector-copy! new-buff 0 old-buff 0 (get-used mutable-vec))
        (set-buff! mutable-vec new-buff)))
    
    (define (argument-is-positive-integer? arg)
      (and (integer? arg)
           (> arg 0)))))
