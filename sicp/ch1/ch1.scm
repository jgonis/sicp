;;java -jar kawa.jar -s -r7rs
;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
;; (import (gauche base)) to get access to debugging and profiling
;; functions
;; (define (load-func)
;;   (load "/home/jgonis/code/sicp/sicp/ch1/ch1.scm")
;;   (load "/home/jgonis/code/sicp/sicp/libs/fp-compare.scm")
;;   (load "/home/jgonis/code/sicp/sicp/tests/ch1/ch1Tests.scm"))

(define-library (ch1 ch1)
  (export ex1-2
          ex1-3
          j-abs
          j-sqrt
          alt-j-sqrt
          ex1-8)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
          (libs fp-compare))
  (begin    
    (define (sum-of-squares x y)
      (+ (square x) (square y)))

    (define (ex1-2)
      (/ (+ 5
            4
            (- 2
               (- 3
                  (+ 6 4/5))))
         (* 3
            (- 6 2)
            (- 2 7))))
    
    (define (ex1-3 x y z)
      (cond ((and (<= x y) (< x z)) (sum-of-squares y z))
            ((and (< y z) (<= y x)) (sum-of-squares x z))
            (else (sum-of-squares x y))))
    
    (define (j-abs x)
      (cond ((< x 0) (- x))
            (else x)))

    (define (j-sqrt x)
      (define (good-enough-direct-compare? guess x)
        (fp-eq? (square guess) x))
      (j-square-root x good-enough-direct-compare?))
    
    (define (alt-j-sqrt x)
      (j-square-root x good-enough-guess-difference?))

    (define (improve-guess guess x)
      (define (j-average x y)
        (/ (+ x y)
           2))
      (j-average guess (/ x guess)))
    
    ;;Exercise 1.7
    (define good-enough-guess-difference?
      (case-lambda
       ((guess x)
	(good-enough-guess-difference? guess x improve-guess))
       ((guess x improve-func)
	(let* ((new-guess (improve-func guess x))
	       (guess-diff (abs (- new-guess guess)))
	       (guess-fraction (/ guess 100000000.0)))
	  (<= guess-diff guess-fraction)))))
    
    (define (j-square-root x good-enough-func?)
      (define (sqrt-iter x guess iterations)
        (cond ((good-enough-func? guess x) guess)
	      (else (sqrt-iter x
			       (improve-guess guess x)
			       (+ iterations 1)))))
      (sqrt-iter x 1.0 1))
    
    (define (ex1-6)
      (let ((a "the new-if fails if passed functions as then or else-clauses because it tries")
            (b "to evaluate them as part of applicative-order evaluations. So the function")
            (c "goes into an infinite loop of calling itself"))))

    (define (ex1-8 x)
      (define (cube-root-iter x guess)
        (cond ((good-enough-guess-difference? guess
					      x
					      improve-guess-cube-root)
	       guess)
	      (else (cube-root-iter x
				    (improve-guess-cube-root guess
							     x)))))
      (define (improve-guess-cube-root guess x)
        (/ (+ (/ x (square guess))
	      (* 2 guess))
           3))
      (cube-root-iter x 1.0))))
