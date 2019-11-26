(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch12)
  (export multirember
          multirember-f
          lr-member?
          lr-union)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define multirember
      (lambda (a lat)
        ((letrec ((mr (lambda (lat)
                        (cond ((null? lat) '())
                              ((eq? a (car lat)) (mr (cdr lat)))
                              (else (cons (car lat) (mr (cdr lat))))))))
           mr)
         lat)))
    (define multirember-f
      (lambda (test?)
        (lambda (a lat)
          (letrec ((multirember
                    (lambda (lat)
                      (cond ((null? lat) '())
                            ((test? a (car lat)) (multirember (cdr lat)))
                            (else (cons (car lat)
                                        (multirember (cdr lat))))))))
            (multirember lat)))))
    (define lr-member?
      (lambda (a lat)
        (letrec ((yes? (lambda (l)
                         (cond ((null? l) #f)
                               ((eq? a (car l)) #t)
                               (else (yes? (cdr l)))))))
          (yes? lat))))
    (define lr-union
      (lambda (set1 set2)
        (letrec ((union-helper
                  (lambda (set)
                    (cond ((null? set) set2)
                          ((member-helper? (car set) set2)
                           (union-helper (cdr set)))
                          (else (cons (car set)
                                      (union-helper (cdr set)))))))
                 (member-helper?
                  (lambda (a lat)
                    (letrec ((mr (lambda (l)
                                   (cond ((null? l) #f)
                                         ((eq? a (car l)) #t)
                                         (else (mr (cdr l)))))))
                      (mr lat)))))
          (union-helper set1))))))

