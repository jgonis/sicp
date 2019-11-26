(include "../littleSchemer/littleSchemerCh1.sld")
(include "../littleSchemer/littleSchemerCh2.sld")
(include "../littleSchemer/littleSchemerCh4.sld")
(define-library (seasoned-schemer ch11)
  (export letcc
          try
          two-in-a-row?
          sum-of-prefixes
          scramble)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch2)
          (little-schemer ch4))
  (begin
    (define-syntax letcc 
      (syntax-rules () 
        ((letcc var body ...) 
         (call-with-current-continuation 
          (lambda (var)  body ... ))))) 
    (define-syntax try 
      (syntax-rules () 
        ((try var a . b) 
         (letcc success 
                (letcc var (success a)) . b))))
    (define is-first?
      (lambda (a lat)
        (cond ((null? lat) #f)
              (else (eq? a (car lat))))))
    (define is-first-b?
      (lambda (a lat)
        (cond ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (two-in-a-row? lat)))))
    (define two-in-a-row?
      (lambda (lat)
        (letrec ((two-in-a-row-helper
                  (lambda (a lat)
                    (cond  ((null? lat) #f)
                           ((eq? a (car lat)) #t)
                           (else (two-in-a-row-helper (car lat)
                                                      (cdr lat)))))))
          (cond ((null? lat) #f)
                (else (two-in-a-row-helper (car lat) (cdr lat)))))))
    (define two-in-a-row-b?
      (lambda (a lat)
        (cond ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else (two-in-a-row-b? (car lat) (cdr lat))))))
    (define sum-of-prefixes
      (lambda (tup)
        (letrec ((sum-of-prefixes-helper
                  (lambda (sum tup)
                    (cond ((null? tup) '())
                          (else (cons (+ sum (car tup))
                                      (sum-of-prefixes-helper
                                       (+ sum (car tup))
                                       (cdr tup))))))))
          (cond ((null? tup) '())
                (else (sum-of-prefixes-helper 0 tup))))))
    (define sum-of-prefixes-b
      (lambda (sum tup)
        (cond ((null? tup) '())
              (else (cons (+ sum (car tup))
                          (sum-of-prefixes-b (+ sum (car tup))
                                             (cdr tup)))))))
    (define scramble
      (lambda (tup)
        (letrec ((scramble-helper
                  (lambda (reversed-prefix tup)
                    (cond ((null? tup) '())
                          (else (cons (pick-helper (car tup)
                                                   (cons (car tup)
                                                         reversed-prefix))
                                      (scramble-helper
                                       (cons (car tup) reversed-prefix)
                                       (cdr tup)))))))
                 (pick-helper (lambda (n lat)
                                (cond ((one? n) (car lat))
                                      (else (pick-helper (sub1 n)
                                                         (cdr lat)))))))
          (cond ((null? tup) '())
                (else (scramble-helper '()
                                       tup))))))
    (define scramble-b
      (lambda (reversed-prefix tup)
        (cond ((null? tup) '())
              (else (cons (pick (car tup) (cons (car tup) reversed-prefix))
                          (scramble-b (cons (car tup) reversed-prefix)
                                      (cdr tup)))))))))

