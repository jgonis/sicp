(define-library (tests ch1 ch1Tests)
  (export ch1-tests
          check-exception)
  (import (scheme base)
          (scheme write)
          (scheme case-lambda)
          (ch1 ch1)
	  (libs fp-compare)
          (srfi 64))
  (begin    
    (define ch1-tests
      (lambda ()
        (ex1-2-tests)
        (ex1-3-tests)
        (j-abs-tests)
        (sqrt-tests j-sqrt "sqrt tests")
        (sqrt-tests alt-j-sqrt "alt-sqrt tests")
        ;;(ex1-8-tests)
        ;;If using Gauche scheme, uncomment this line to avoid the
        ;;test count continuing to increase
        (test-runner-reset (test-runner-current))))

    (define (ex1-2-tests)
      (test-begin "ex1-2")
      (test-equal (ex1-2) -37/150)
      (test-end "ex1-2"))

    (define (ex1-3-tests)
      (test-begin "ex1-3")
      (test-equal (ex1-3 1 2 3) 13)
      (test-equal (ex1-3 1 3 2) 13)
      (test-equal (ex1-3 3 2 1) 13)
      (test-equal (ex1-3 3 1 2) 13)
      (test-equal (ex1-3 3 2 2) 13)
      (test-equal (ex1-3 2 3 2) 13)
      (test-equal (ex1-3 2 2 3) 13)
      (test-equal (ex1-3 3 3 3) 18)
      (test-end "ex1-3"))

    (define (j-abs-tests)
      (test-begin "j-abs tests")
      (test-equal (j-abs 1) 1)
      (test-equal (j-abs 0) 0)
      (test-equal (j-abs -1) 1)
      (test-end "j-abs tests"))

    (define (sqrt-tests sqrt-func test-name)
      (test-begin test-name)
      (test-assert (fp-eq? 2
			   (sqrt-func 4)))
      (test-assert (fp-eq? 0.0000009
                           (sqrt-func 0.00000000000081)
                           0.001))
      (test-assert (fp-eq? 985881
			   (sqrt-func 971961346161)
			   0.001))
      (test-end test-name))

    (define (ex1-8-tests)
      (test-begin "ex1-8 tests")
      ;; (let* ((test-val 8)
      ;;        (test-result (ex1-8 test-val)))
      ;;   (test-assert (fp-equal? 2.0 test-result)))
      ;; (let* ((test-val 0.001)
      ;;        (test-result (ex1-8 test-val)))
      ;;   (test-assert (fp-equal? 0.1 test-result)))
      (test-end "ex1-8 tests"))))
