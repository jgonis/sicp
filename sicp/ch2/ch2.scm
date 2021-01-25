;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions

(define-library (ch2 ch2)
  (export test-func)
  (import (scheme base)
          (scheme write)
	  (libs fp-compare)
	  (libs helpers)
	  (ch2 rationalNumbers))
  (begin
    (define (test-func) 1)))
