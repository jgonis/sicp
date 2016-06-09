(define-library (sicp ch1)
  (export square sum-of-squares abs ex1.3)
  (import (except (scheme base) square abs))
  (begin
    (define square
      (lambda (n)
        (* n n)))
    (define sum-of-squares
      (lambda (x y)
        (+ (square x) (square y))))
    (define abs
      (lambda (x)
        (cond ((< x 0) (- x))
              (else x))))
    (define ex1.3
      (lambda (x y z)
        (cond ((and (>= x y) (>= x z))
               (cond ((> z y) (sum-of-squares x z))
                     (else (sum-of-squares x y))))
              ((and (>= y x) (>= y z))
               (cond ((> x z) (sum-of-squares y x))
                     (else (sum-of-squares y z))))
              ((and (>= z x) (>= z y))
               (cond ((> x y) (sum-of-squares z x))
                     (else (sum-of-squares z y)))))))))

(import (srfi :78))
(import (rename (prefix (sicp ch1) sicp-)))
(check-set-mode! 'summary)
(check (sicp-square 5) => 25)
(check (sicp-sum-of-squares 3 4) => 25)
(check (sicp-abs -1) => 1)
(check (sicp-abs 1) => 1)
(check (sicp-ex1.3 1 2 3) => 13)
(check (sicp-ex1.3 3 2 1) => 13)
(check (sicp-ex1.3 3 1 2) => 13)
(check (sicp-ex1.3 3 3 2) => 18)
(check (sicp-ex1.3 3 2 3) => 18)
(check (sicp-ex1.3 2 3 3) => 18)
(check (sicp-ex1.3 3 3 3) => 18)
(check-report)
(check-reset!)

;exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;exercise 1.4
;The evaluation of the function starts with the first compound
;expression which evaluates the if statement and returns either
;the plus operator or the minus operator and the applies that
;to the values of the following two expressions, which are
;the numbers themselves.

;exercise 1.5
;In an applicative order evaluation system Ben B.'s test will
;result in an infinite loop as the system tries to evaluate the
;argument (p) which is an infinitely recursive function. In a normal
;order system the functions will be fully expanded and the if test
;will short circuit past trying to evaluate p, and just return 0.

;Note for discussion: The distinction made between functions and
;procedures wherein functions are declarative and descriptive whereas
;procedures are effectives, and this is the root of the paradigm shift
;in thinking that computer programming engenders, ie it is about
;"how-to" knowledge, not what is.

(define sicp-sqrt-iter
  (lambda (guess x test-func improve-func)
    (cond ((test-func guess x) guess)
          (else (sicp-sqrt-iter (improve-func guess x)
                                x
                                test-func
                                improve-func)))))

(define sicp-sqrt
  (lambda (x)
    (define improve-guess
      (lambda (guess x)
        (define average
          (lambda (x y)
            (/ (+ x y) 2)))
        (average guess (/ x guess))))
    (define good-enough?
      (lambda (guess x)
        (< (abs (- (square guess) x)) 0.000000000000001)))
    (sicp-sqrt-iter 1.0 x good-enough? improve-guess)))

;Exercise 1.6
;The new-if program enters into an infinite recursion because it
;attempts to evaluate the arguments to the "new-if" function, before it
;actually calls the "new-if" function, which means it never actually
;test the guess for being "good-enough".

;Exercise 1.7
(define alt-sicp-sqrt
  (lambda (x)
    (define alt-good-enough?
      (lambda guess x)
      #t)
    sicp-sqrt-iter 1.0 x alt-good-enough?))


;Exercise 1.8
(define sicp-cube-root
  (lambda (x)
    (define improve-guess
      (lambda (guess x)
        (define average
          (lambda (x y)
            (/ (+ x y) 3)))
        (average (* 2 guess) (/ x (* guess guess)))))
    (define good-enough?
      (lambda (guess x)
        (< (abs (- (* guess guess guess) x)) 0.000000000000001)))
    (sicp-sqrt-iter 1.0 x good-enough? improve-guess)))
