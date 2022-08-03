(define-library (seasoned-schemer ch19)
  (export six-layers
          four-layers
          deepB
          toppings
          deep&co
          deep&coB
          two-in-a-row*
          walk)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define six-layers
      (lambda (p)
        (cons
         (cons
          (cons
           (cons
            (cons
             (cons p '())
             '())
            '())
           '())
          '())
         '())))

    (define four-layers
      (lambda (p)
        (cons
         (cons
          (cons
           (cons p '())
           '())
          '())
         '())))

    (define toppings '())

    (define deepB
      (lambda (m)
        (cond ((= 0 m) (letcc jump
                              (set! toppings jump)
                              'pizza))
              (else (cons (deepB (- m 1)) '())))))

    (define deep&co
      (lambda (m k)
        (cond ((= 0 m) (k 'pizza))
              (else (deep&co (- m 1)
                             (lambda (x)
                               (k (cons x '()))))))))

    (define deep&coB
      (lambda (m k)
        (cond ((= 0 m) (let ()
                         (set! toppings k)
                         (k 'pizza)))
              (else (deep&coB (- m 1)
                              (lambda (x)
                                (k (cons x '()))))))))

    (define two-in-a-row*
      (lambda (l)
        #f))

    (define walk
      (lambda (l)
        (cond ((null? l) '())
              ((atom? (car l)) (leave (car l)))
              (else (begin
                      (walk (car l))
                      (walk (cdr l)))))))
    ;; Uhh, come back to this. Continuations are wild.
    (define waddle
      (lambda (l)
        (cond ((null? l) '())
              (else l))))))
