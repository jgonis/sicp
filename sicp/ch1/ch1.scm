;;java -jar kawa.jar -s -r7rs
;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
(define-library (ch1 ch1)
  (export ex1-2
          ex1-3
          j-abs
          j-sqrt
          alt-j-sqrt
          SQRT_TOLERANCE
          ex1-8
          fp-equal?)
  (import (scheme base)
          (scheme write))
  (begin
    (define SQRT_TOLERANCE 0.001)
    (define FP_EQUAL_TOLERANCE 0.0000001)
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

    (define (fp-equal? x y)
      (let ((diff (abs (- x y))))
        (cond ((< diff FP_EQUAL_TOLERANCE) #t)
              (else #f))))

    (define (j-sqrt x)
      (define (good-enough-direct-compare? guess x)
        (fp-equal? (square guess) x))
      (j-square-root x good-enough-direct-compare?))
    
    (define (alt-j-sqrt x)
      (j-square-root x good-enough-guess-difference?))

    (define (improve-guess guess x)
      (define (j-average x y)
        (/ (+ x y)
           2))
      (j-average guess (/ x guess)))
    
    ;;Exercise 1.7
    (define (good-enough-guess-difference? guess x)
      (let* ((new-guess (improve-guess guess x))
             (guess-diff (abs (- new-guess guess))))
        (= guess-diff 0.0)))
    
    (define (j-square-root x good-enough-func?)
      (define (sqrt-iter x guess iterations)
        (cond ((good-enough-func? guess x) guess)
              (else (sqrt-iter x (improve-guess guess x) (+ iterations 1)))))
      (sqrt-iter x 1.0 1))
    
    (define (ex1-6)
      (let ((a "the new-if fails if passed functions as then or else-clauses because it tries")
            (b "to evaluate them as part of applicative-order evaluations. So the function")
            (c "goes into an infinite loop of calling itself"))))

    (define (ex1-8 x)
      (define (cube-root-iter guess)
        (cond ((good-enough-guess-difference? guess x) guess)
              (else (cube-root-iter (improve-guess guess)))))
      (define (improve-guess guess)
        (/ (+ (/ x (square guess))
              (* 2 guess))
           3))
      (cube-root-iter 1.0))))




(define (load-func)
  (load "ch1/ch1.scm")
  (load "tests/ch1/ch1Tests.scm"))
