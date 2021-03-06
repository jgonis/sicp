(include "seasonedSchemerCh11.sld")
(define-library (seasoned-schemer ch13)
  (export lr-intersect
          lr-intersectall
          lr-rember
          rember-beyond-first
          rember-upto-last)
  (import (scheme base)
          (scheme write)
          (seasoned-schemer ch11))
  (begin
    (define lr-intersect
      (lambda (set1 set2)
        (letrec ((member?
                  (lambda (a lat)
                    (letrec ((helper
                              (lambda (l)
                                (cond ((null? l) #f)
                                      ((eq? a (car l)) #t)
                                      (else (helper (cdr l)))))))
                      (helper lat))))
                 (i-helper
                  (lambda (set)
                    (cond ((null? set) '())
                          ((member? (car set) set2)
                           (cons (car set)
                                 (i-helper (cdr set))))
                          (else (i-helper (cdr set)))))))
          (cond ((null? set2) '())
                (else (i-helper set1))))))
    (define lr-intersectall
      (lambda (lset)
        (letcc hop
               (letrec
                   ((iall-helper
                     (lambda (lset)
                       (cond ((null? (car lset)) (hop (quote ())))
                             ((null? (cdr lset)) (car lset))
                             (else (intersect (car lset)
                                              (iall-helper
                                               (cdr lset)))))))
                    (member?
                     (lambda (a lat)
                       (cond ((null? lat) #f)
                             ((eq? a (car lat)) #t)
                             (else (member? a (cdr lat))))))
                    (intersect
                     (lambda (set1 set2)
                       (letrec
                           ((i-helper
                             (lambda (set)
                               (cond ((null? set) '())
                                     ((member? (car set) set2)
                                      (cons (car set)
                                            (i-helper (cdr set))))
                                     (else (i-helper (cdr set)))))))
                         (cond ((null? set2) (hop '()))
                               (else (i-helper set1)))))))
                 (cond ((null? lset) '())
                       (else (iall-helper lset)))))))
    (define lr-rember
      (lambda (a lat)
        (letcc hop
               (letrec ((helper
                         (lambda (lat)
                           (cond ((null? lat) '())
                                 ((eq? a (car lat)) (cdr lat))
                                 (else (cons (car lat)
                                             (helper (cdr lat))))))))
                 (helper lat)))))
    (define rember-beyond-first
      (lambda (a lat)
        (letrec ((helper
                  (lambda (lat)
                    (cond ((or (null? lat)
                               (eq? a (car lat))) '())
                          (else (cons (car lat)
                                      (helper (cdr lat))))))))
          (helper lat))))
    (define rember-upto-last
      (lambda (a lat)
        (letcc skip
               (letrec ((helper
                         (lambda (lat)
                           (cond ((null? lat) '())
                                 ((eq? (car lat) a)
                                  (skip (helper (cdr lat))))
                                  (else (cons (car lat)
                                              (helper (cdr lat))))))))
                 (helper lat)))))))
