;; A centralized place to create a function that will load all of the files in the
;; system that comprise my SICP work so I can work in an iterative fashion
(define (load-func)
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch1/ch1.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/ch2.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/rationalNumbers.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/point.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/segment.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/rectangle.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/rectangleFunctions.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/altRectangle.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/intervals.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/altIntervals.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/intervalFunctions.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/ch2/tree-library.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/libs/fp-compare.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/libs/helpers.scm")
  ;;Test files
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch1/ch1Tests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/ch2Tests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/pointTests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/segmentTests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/rectangleTests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/intervalTests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/rationalNumberTests.scm")
  (load "/Users/jeff.gonis/Code/sicp/sicp/tests/ch2/treeTests.scm"))
