;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions

(define-library (ch2 ch2)
  (export alt-cons
	  alt-car
	  alt-cdr
	  numeric-cons
	  numeric-car
	  numeric-cdr)
  (import (scheme base)
          (scheme write)
	  (libs fp-compare)
	  (libs helpers)
	  (ch2 rationalNumbers))
  (begin    
    (define (alt-cons x y)
      (lambda (m) (m x y)))
    
    (define (alt-car pair)
      (pair (lambda (x y) x)))
    
    (define (alt-cdr pair)
      (pair (lambda (x y) y)))
    
    (define (numeric-cons x y)
      (* (expt 2 x) (expt 3 y)))
    
    (define (numeric-car pair)
      (define (iter num counter)
	(cond ((even? num) counter)
	      (else (iter (/ num 2) (+ counter 1)))))
      (iter pair 0))
    
    (define (numeric-cdr pair)
      (define (iter num counter)
	(cond ((not (= 0 (remainder num 3))) counter)
	      (else (iter (/ num 3) (+ counter 1)))))
      (iter pair 0))))
