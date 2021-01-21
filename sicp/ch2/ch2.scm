;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions
;; (define (load-func)
;;   (load "/home/jgonis/code/sicp/sicp/ch1/ch1.scm")
;;   (load "/home/jgonis/code/sicp/sicp/ch2/ch2.scm")
;;   (load "/home/jgonis/code/sicp/sicp/ch2/rationalNumbers.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/fp-compare.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/helpers.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch1/ch1Tests.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch2/ch2Tests.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch2/rationalNumberTests.scm"))

(define-library (ch2 ch2)
  (export test-func)
  (import (scheme base)
          (scheme write)
	  (libs fp-compare)
	  (libs helpers)
	  (ch2 rationalNumbers))
  (begin
    (define (test-func) 1)))
