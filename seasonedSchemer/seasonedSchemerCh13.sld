(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch13)
  (export lr-intersect
          lr-intersectall)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define lr-intersect
      (lambda (set1 set2)
        (letrec ((member? (lambda (a lat)
                            (letrec ((helper
                                      (lambda (l)
                                        (cond ((null? l) #f)
                                              ((eq? a (car l)) #t)
                                              (else (helper (cdr l)))))))
                              (helper lat))))
                 (i-helper (lambda (set)
                             (cond ((null? set) '())
                                   ((member? (car set) set2)
                                    (cons (car set)
                                          (i-helper (cdr set))))
                                   (else (i-helper (cdr set)))))))
          (cond ((null? set1) '())
                (else (i-helper set1))))))
    (define lr-intersectall
      (lambda (lset)
        (letrec ((helper
                  (lambda (lset)
                    (cond ((null? (cdr lset)) (car lset))
                          (else (lr-intersect (car lset)
                                              (helper (cdr lset))))))))
          (cond ((null? lset) '())
                (else (helper lset))))))))
