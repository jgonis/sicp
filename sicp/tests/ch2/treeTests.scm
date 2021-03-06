(define-library (tests ch2 treeTests)
  (export tree-tests)
  (import (scheme base)
	  (ch2 tree-library)
	  (srfi 64))
  (begin
    (define (tree-tests)
      (count-leaves-tests)
      (deep-reverse-tests)
      (fringe-tests)
      (test-runner-reset (test-runner-current)))

    (define (count-leaves-tests)
      (test-begin "count-leaves")
      (let ((x (cons (list 1 2) (list 3 4))))
	(test-equal 4 (count-leaves x))
	(test-equal 8 (count-leaves (list x x))))
      (test-end "count-leaves"))

    (define (deep-reverse-tests)
      (test-begin "deep-reverse")
      (test-equal '() (deep-reverse '()))
      (test-equal 3 (deep-reverse 3))
      (let ((input (list (list 1 2) (list 3 4)))
	    (expected (list (list 4 3) (list 2 1))))
	(test-equal expected (deep-reverse input)))
      (test-end "deep-reverse"))

    (define (fringe-tests)
      (test-begin "fringe-tests")
      (let ((x (list (list 1 2) (list 3 4)))
	    (x-x (list x x)))
	(test-equal (list 1 2 3 4) (fringe x))
	(test-equal (list 1 2 3 4 1 2 3 4) (fringe x-x)))
      (test-end "fringe-tests"))))
