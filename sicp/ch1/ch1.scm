;;java -jar kawa.jar -s -r7rs
;;"/home/jeff.gonis/Code/gauche/bin/gosh -i -r7 -I /home/jeff.gonis/Code/sicp/sicp"
(define-library (ch1 ch1)
  (export ex1-2
          ex1-3
          j-abs
          j-sqrt
          SQRT_TOLERANCE)
  (import (scheme base)
          (scheme write))
  (begin
    (define SQRT_TOLERANCE 0.001)
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
        (cond ((< diff  SQRT_TOLERANCE) #t)
              (else #f))))

    (define (j-sqrt x)
      (define (sqrt-iter x guess iterations)
        (cond ((good-enough? guess x) (display "iterations: ")
               (display iterations)
               (newline)
               guess)
              (else (sqrt-iter x (improve-guess guess x) (+ iterations 1)))))
      (define (improve-guess guess x)
        (average guess (/ x guess)))
      (define (good-enough? guess x)
        (fp-equal? (square guess) x))
      (define (alt-good-enough? guess x)
        (let* ((new-guess (improve-guess guess x))
               (guess-diff (abs (- new-guess guess))))
          (< guess-diff SQRT_TOLERANCE)))
      (define (average x y)
        (/ (+ x y) 2))
      (sqrt-iter x 1.0 1))

    (define (ex1-6)
      (let ((a "the new-if fails if passed functions as then or else-clauses because it tries")
            (b "to evaluate them as part of applicative-order evaluations. So the function")
            (c "goes into an infinite loop of calling itself"))))))


