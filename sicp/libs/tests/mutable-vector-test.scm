(define-library (libs tests mutable-vector-test)
  (export run-tests)
  (import (scheme base)
          (scheme write)
          (srfi 64)
          (libs mutable-vector)
          (libs mutable-vector-messages))
  (begin
    (define (run-tests)
      (make-mutable-vec-test)
      (add-element-test)
      (mutable-vector-ref-test)
      (remove-element-test)
      ;;If using Gauche scheme, uncomment this line to avoid the test count continuing to increase
      (test-runner-reset (test-runner-current)))
    
    (define (make-mutable-vec-test)
      (test-begin "make-mutable-vector-tests")
      (let* ((capacity 10)
             (mv (make-mutable-vector))
             (mv-preallocated (make-mutable-vector capacity)))
        (test-assert (mutable-vector? mv))
        (test-assert (mutable-vector? mv-preallocated))
        (test-equal 0 (mutable-vector-length mv))
        (test-equal 0 (mutable-vector-length mv-preallocated))
        (test-equal capacity (get-capacity mv-preallocated))
        (test-for-error "make vector with negative capacity"
                        (lambda () (make-mutable-vector -1))
                        INVALID_CAPACITY_ERROR)
        (test-for-error "make vector with non-integer capacity"
                        (lambda () (make-mutable-vector 2))
                        INVALID_CAPACITY_ERROR))
      (test-end "make-mutable-vector-tests"))

    (define (add-element-test)
      (test-begin "add-element-tests")
      (let ((mv (make-mutable-vector)))
        (add-element mv "a")
        (test-assert "add element to empty vector puts it at index 0"
                     (string=? "a" (mutable-vector-ref mv 0)))
        (add-element mv "b")
        (test-assert "add element to vector with 1 element puts it at index 1" (string=? "b" (mutable-vector-ref mv 1)))
                (add-element mv "c")
        (test-assert "add element to vector with 1 element puts it at index 1" (string=? "c" (mutable-vector-ref mv 2)))
                (add-element mv "d")
        (test-assert "add element to vector with 1 element puts it at index 1" (string=? "d" (mutable-vector-ref mv 3)))
                (add-element mv "e")
        (test-assert "add element to vector with 1 element puts it at index 1" (string=? "e" (mutable-vector-ref mv 4))))
      (test-end "add-element-tests"))
    
    (define (mutable-vector-ref-test)
      (test-begin "mutable-vector-ref-tests")
      (let ((mv (make-mutable-vector)))
        (add-element mv "a")
        (test-for-error "-1 index is out of bounds"
                        (lambda () (mutable-vector-ref mv -1))
                        INVALID_INDEX_ERROR)
        (test-for-error "index greater than number of items contained is out of bounds"
                        (lambda () (mutable-vector-ref mv (+ 1 (mutable-vector-length mv))))
                        INVALID_INDEX_ERROR)
        (test-assert "can access data correctly" (string=? "a" (mutable-vector-ref mv 0))))
      (test-end "mutable-vector-ref-tests"))

    (define (remove-element-test)
      (test-begin "remove-element-tests")
      (let ((mv (make-mutable-vector)))
        (add-element mv "a")
        (add-element mv "b")
        (test-equal (mutable-vector-length mv) 2)
        (test-for-error "test removal with negative index"
                        (lambda () (remove-element mv -1))
                        INVALID_INDEX_ERROR)
        (test-for-error "test removal with invalid index"
                        (lambda () (remove-element mv 10))
                        INVALID_INDEX_ERROR)
        (let ((removed-element (remove-element mv 1)))
          (test-equal removed-element "b")
          (test-equal 1 (mutable-vector-length mv)))
        ;(test-assert (string=? "a" (mutable-vector-ref 0)))
        ;(add-element mv "b")
        (test-end "remove-element-tests")))


    ;;Todo: make this fail if no error occurs when calling thunk
    (define (test-for-error description thunk error-string)
      (test-assert description
                   (guard (condition
                           (else (and (error-object? condition)
                                      (string=? (error-object-message condition)
                                                error-string))))
                          (thunk))))))

;;(import (libs tests mutable-vector-test))
;;(run-tests)

