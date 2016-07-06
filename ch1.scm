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

;Some examples of numbers that our sqrt procedure doesn't work that
;well on:

(define recursive-factorial
  (lambda (n)
    (cond ((= n 1) 1)
          (else (* n (recursive-factorial (- n 1)))))))

(define iter-factorial
  (lambda (n)
    (define factorial-helper
      (lambda (product counter max-count)
        (cond ((> counter max-count) product)
              (else (factorial-helper (* counter product)
                                      (+ counter 1)
                                      max-count)))))
    (factorial-helper 1 1 n)))


;Exercise 1.9
;The first procedure defines a recursive process because the inc procedure
;will need to wait on the result of calling the '+' procedure before it
;can carry out its operations, and so on, for each value of a.
;
;The second procedure defines an iterative process because the '+'
;procedure call just pass in the new argument values for a and b and
;does not need to wait on any other evaluations of itself.
;
;The first method will generate this process
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
;
;The second process will generate the following process
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(0 9)
;9

;Exercise 1.10
;(A 1 10) -> 1024
;(A 2 4) -> 65536
;(define (f n) (A 0 n) -> this defines 2 * n
;(define (g n) (A 1 n) -> this defines the function 2^n
;(define (h n) (A 2 n) -> this defines the function

(define count-change
  (lambda (amount)
    (define cc
      (lambda (amount kinds-of-coins)
        (cond ((= amount 0) 1)
              ((or (< amount 0)
                   (= kinds-of-coins 0))
               0)
              (else (+ (cc amount (- kinds-of-coins 1))
                       (cc (- amount (first-denomination
                                      kinds-of-coins))
                           kinds-of-coins))))))
    (define first-denomination
      (lambda (kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
              ((= kinds-of-coins 2) 5)
              ((= kinds-of-coins 3) 10)
              ((= kinds-of-coins 4) 25)
              ((= kinds-of-coins 5) 50))))
    (cc amount 5)))

;exercise 1.12
(define pascals-triangle
  (lambda (row column)
    (define pascal-helper
      (lambda (row column)
        (cond ((or (= column 1) (= column row)) 1)
              (else (+ (pascal-helper (- row 1) (- column 1))
                       (pascal-helper (- row 1) column))))))
    (cond ((< column 1) (error "column value cannot be less than 1" column))
          ((< row 1) (error "row value cannot be less than 1" row))
          ((> column row) (error "column value cannot exceed row value" column row))
          (else (pascal-helper row column)))))

(define recursive-factorial
  (lambda (n)
    (cond ((= n 1) 1)
          (else (* n (recursive-factorial (- n 1)))))))

(define iter-factorial
  (lambda (n)
    (define helper
      (lambda (total count)
        (cond ((= count 1) total)
              (else (helper (* count total) (- count 1))))))
    (helper 1 n)))

(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2)))))))

(define iter-fib
  (lambda (n)
    (define helper
      (lambda (a b count)
        (cond ((= count 0) b)
              (else (helper b (+ a b) (- count 1))))))
    (helper 0 1 n)))

(define-library (sicp ex115)
  (export sine)
  (import (scheme base))
  (begin
    (define cube
      (lambda (x)
        (* x x x)))
    (define p
      (lambda (x)
        (- (* 3 x)
           (* 4 (cube x)))))
    (define sine
      (lambda (angle)
        (cond ((not (> (abs angle) 0.1)) angle)
              (else (p (sine (/ angle 3.0)))))))))
;How many times is the procedure p applied when sine 12.15 is evaluated?
; :> sine -> p (/ 12.15 3) -> sine -> p (/ 4.05 3) -> sine -> p (/ 1.34999 3)
; -> sine -> p (/ .449999 3) -> sine -> p (/ .15 3) -> sine
; So P is executed 5 times.

; The growth in both space and time is linear as angle increases as it
; decreases by a constant amount (divide by 3) with each step, and it is
; recursive, so the space grows for each step as well.

(define jeff-expt
  (lambda (a n)
    (cond ((= n 0) 1)
          (else (* a (jeff-expt a (- n 1)))))))

(define jeff-linear-expt
  (lambda (a n)
    (define helper
      (lambda (a n product)
        (cond ((= n 0) product)
              (else (helper a (- n 1) (* a product))))))
    (helper a n 1)))

(define jeff-fast-expt
  (lambda (a n)
    (cond ((= n 0) 1)
          ((even? n) (square (jeff-fast-expt a (/ n 2))))
          (else (* a (jeff-fast-expt a (- n 1)))))))

;Exercise 1.16
(define jeff-really-fast-expt
  (lambda (a n)
    (cond ((= n 0) 1)
          ((even? n) (jeff-really-fast-expt (square a) (/ n 2)))
          (else (* a (jeff-really-fast-expt a (- n 1)))))))
(define jeff-fast-iter-expt
  (lambda (a n)
    (define iter-helper
      (lambda (a b n)
        (cond ((= n 0) a)
              ((even? n) (iter-helper a (square b) (/ n 2)))
              (else (iter-helper (* a b) b (- n 1))))))
    (iter-helper 1 a n)))

;Ex 1.17
(define jeff-fast-*
  (lambda (a b)
    (define double
      (lambda (a)
        (* a 2)))
    (define halve
      (lambda (a)
        (/ a 2)))
    (cond ((= b 0) 0)
          ((even? b) (double (jeff-fast-* a (halve b))))
          (else (+ a (jeff-fast-* a (- b 1)))))))

;Section 1.2.5
(define jeff-gcd
  (lambda (a b)
    (cond ((= b 0) a)
          (else (jeff-gcd b (remainder a b))))))
