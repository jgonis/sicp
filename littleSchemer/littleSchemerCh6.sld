(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
(define-library (little-schemer ch6)
  (export numbered?
          value
          prefix-value
          sero?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4))
  (begin
    (define numbered?
      (lambda (aexp)
        (cond ((number? aexp) #t)
              ((null? aexp) #t)
              ((atom? (car aexp))
               (cond ((number? (car aexp)) (numbered? (cdr aexp)))
                     ((eq? '+ (car aexp)) (numbered? (cdr aexp)))
                     ((eq? '* (car aexp)) (numbered? (cdr aexp)))
                     ((eq? '^ (car aexp)) (numbered? (cdr aexp)))
                     (else #f)))
              (else (and (numbered? (car aexp))
                         (numbered? (cdr aexp)))))))
    (define 1st-sub-exp
      (lambda (aexp)
        (car (cdr aexp))))
    (define 2nd-sub-exp
      (lambda (aexp)
        (car (cdr (cdr aexp)))))
    (define operator
      (lambda (aexp)
        (car aexp)))
    (define value
      (lambda (aexp)
        (cond ((atom? aexp) aexp)
              ((eq? '+ (car (cdr aexp)))
               (j+ (value (car aexp))
                   (value (2nd-sub-exp aexp)))) 
              ((eq? '* (car (cdr aexp)))
               (j* (value (car aexp))
                   (value (2nd-sub-exp aexp))))
              ((eq? '^ (car (cdr aexp)))
               (^ (value (car aexp))
                  (value (2nd-sub-exp aexp)))))))
    (define prefix-value
      (lambda (aexp)
        (cond ((atom? aexp) aexp)
              ((eq? '+ (operator aexp))
               (j+ (prefix-value (1st-sub-exp aexp))
                   (prefix-value (2nd-sub-exp aexp))))
              ((eq? '* (operator aexp))
               (j* (prefix-value (1st-sub-exp aexp))
                   (prefix-value (2nd-sub-exp aexp))))
              ((eq? '^ (operator aexp))
               (^ (prefix-value (1st-sub-exp aexp))
                  (prefix-value (2nd-sub-exp aexp)))))))
    (define sero?
      (lambda (n)
        (null? n)))))



