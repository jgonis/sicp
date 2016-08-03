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
(define-library (sicp math-funcs)
  (export PI
          cube)
  (import (scheme base))
  (begin
    (define PI 3.141592653589793238462643383279)
    (define (cube x)
      (* x x x))))

(import (sicp math-funcs))

(define-library (sicp testex13)
  (export test-ex13)
  (import (scheme base)
          (srfi 78)
          (rename (prefix (sicp ch1) sicp-)))
  (begin
    (define (test-ex13)
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
      (check-reset!))))

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
      (lambda (guess x) #t))
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
          ((> column row) (error "column value cannot exceed row value" column
                                 row))
          (else (pascal-helper row column)))))

(define pascal-printer
  (lambda (rows-to-print)
    (define helper
      (lambda (number-of-rows current-row)
        (cond ((= current-row number-of-rows)
               (cons (generate-row-string current-row) '()))
              (else (cons (generate-row-string current-row)
                          (helper number-of-rows (+ current-row 1)))))))
    (define print-strings
      (lambda (list-of-strings)
        (cond ((not (null? list-of-strings))
               (display (car list-of-strings))
               (newline)
               (print-strings (cdr list-of-strings))))))
    (define generate-row-string
      (lambda (current-row)
        (define generate-helper
          (lambda (current-row current-column)
            (cond ((= current-column current-row)
                   (number->string (pascals-triangle
                                    current-row
                                    current-column)))
                  (else (string-append
                         (number->string (pascals-triangle
                                          current-row
                                          current-column))
                         " "
                         (generate-helper current-row (+ current-column 1)))))))
        (generate-helper current-row 1)))
    (print-strings (helper rows-to-print 1))))

#|(define pad-string
  (lambda (current-string child-string)
    (let* ((child-length (string-length child-string))
           (current-length (string-length current-string))
           (left-pad (ceiling (/ (- child-length current-length) 2)))
           (right-pad (floor (/ (- child-length current-length) 2)))))))
|#

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

;Exercise 1.18
(define jeff-fast-iter-*
  (lambda (a b)
    (define double
      (lambda (a)
        (* a 2)))
    (define halve
      (lambda (a)
        (/ a 2)))
    (define helper
      (lambda (state a b)
        (cond ((= b 0) state)
              ((even? b) (helper state (double a) (halve b)))
              (else (helper (+ a state) a (- b 1))))))
    (helper 0 a b)))

                                        ;Section 1.2.5
(define jeff-gcd
  (lambda (a b)
    (cond ((= b 0) a)
          (else (jeff-gcd b (remainder a b))))))

(define-library (sicp naive-prime)
  (export prime?)
  (import (scheme base))
  (begin
    (define smallest-divisor
      (lambda (n)
        (find-divisor n 2)))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? n test-divisor) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (define divides?
      (lambda (a b)
        (= (remainder a b) 0)))
    (define prime?
      (lambda (n)
        (= (smallest-divisor n) n)))))

(define-library (sicp fast-prime)
  (export prime?)
  (import (scheme base)
          (srfi 27))
  (begin
    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (square (expmod base (/ exp 2) m))
                        m))
            (else (remainder (* base (expmod base (- exp 1) m))
                             m))))
    (define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random-integer (- n 1)))))
    (define (prime? n times)
      (cond ((= times 0) #t)
            ((fermat-test n)
             (prime? n (- times 1)))
            (else #f)))))

(define-library (sicp test-primes)
  (export test-primes)
  (import (scheme base)
          (srfi 78)
          (rename (prefix (sicp naive-prime) slow-))
          (rename (prefix (sicp fast-prime) fast-)))
  (begin
    (define (test-primes)
      (check-set-mode! 'summary)
      (check (slow-prime? 5) => #t)
      (check (fast-prime? 5 3) => #t)
      (check-report)
      (check-reset!))))

(define-library (sicp ch13)
  (export sum-integers
          sum-cubes
          pi-sum
          iterative-pi-sum
          sum
          integral
          iterative-integral
          iterative-sum
          simpsons-rule)
  (import (scheme base))
  (begin
    (define (integral f a b dx)
      (define (add-dx x) (+ x dx))
      (* (sum f (+ a (/ dx 2.0)) add-dx b)
         dx))
    (define (iterative-integral f a b dx)
      (define (add-dx x) (+ x dx))
      (* (iterative-sum f (+ a (/ dx 2.0)) add-dx b)
         dx))
    (define (iterative-sum term a next b)
      (define (helper term a next b total)
        (cond ((> a b) total)
              (else (helper term (next a) next b (+ total
                                                    (term a))))))
      (helper term a next b 0))
    (define (sum term a next b)
      (cond ((> a b) 0)
            (else (+ (term a)
                     (sum term (next a) next b)))))
    (define (cube a)
      (* a a a))
    (define (sum-integers a b)
      (cond ((> a b) 0)
            (else (+ a (sum-integers (+ a 1) b)))))
    (define (sum-cubes a b)
      (cond ((> a b) 0)
            (else (+ (cube a)
                     (sum-cubes (+ a 1) b)))))
    (define (pi-sum a b)
      (cond ((> a b) 0)
            (else (+ (/ 1.0 (* a (+ a 2)))
                     (pi-sum (+ a 4) b)))))
    (define (iterative-pi-sum a b)
      (define (helper a b total)
        (cond ((> a b) total)
              (else (helper (+ a 4)
                            b
                            (+ total
                               (/ 1.0 (* a (+ a 2))))))))
      (helper a b 0))
    (define (simpsons-rule f a b n)
      ;increment n by 2, avoid duplicating lambdas.
      (define (inc-by-two x)
        (+ x 2))
      ;function value at f(a + kh)
      (define (helper k h)
        (f (+ a (* k h))))
      (let ((h (/ (- b a) n)))
        (* (/ h 3)
           (+ (f a) ;function value at n = 0
              (f b) ;function value at n
              ;function values starting at n = 1, times 4, and continuing
              ;every 2, aka 1, 3, 5, etc
              (iterative-sum (lambda (x) (* 4 (helper x h)))
                             1
                             inc-by-two
                             (- n 1))
              ;function values starting at n = 2, times 2 and continuing
              ;every 2, aka 2, 4, 6, etc
              (iterative-sum (lambda (x) (* 2 (helper x h)))
                             2
                             inc-by-two
                             (- n 2))))))))
