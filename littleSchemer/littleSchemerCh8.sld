(include "littleSchemerCh1.sld")
(define-library (little-schemer ch8)
  (export rember-f
          rember-gen
          insertL-f
          insertR-f
          subst-f
          insert-left
          insert-right
          insert-only-new
          insert-nothing
          insert-gen)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1))
  (begin
    (define rember-f
      (lambda (test? a l)
        (cond ((null? l) '())
              ((test? a (car l)) (cdr l))
              (else (cons (car l)
                          (rember-f test? a (cdr l)))))))
    (define rember-gen
      (lambda (test?)
        (lambda (a l)
          (cond ((null? l) '())
                ((test? a (car l)) (cdr l))
                (else (cons (car l)
                            ((rember-gen test?) a (cdr l))))))))
    (define insertL-f
      (lambda (test?)
        (lambda (new old lat)
          (cond ((null? lat) '())
                ((test? old (car lat))
                 (cons new
                       (cons (car lat) (cdr lat))))
                (else (cons (car lat) ((insertL-f test?)
                                       new
                                       old
                                       (cdr lat))))))))
    (define insertR-f
      (lambda (test?)
        (lambda (new old lat)
          (cond ((null? lat) '())
                ((test? old (car lat)) (cons (car lat)
                                             (cons new
                                                   (cdr lat))))
                (else (cons (car lat) ((insertR-f test?)
                                       new
                                       old
                                       (cdr lat))))))))
    (define subst-f
      (lambda (test?)
        (lambda (new old lat)
          (cond ((null? lat) '())
                ((test? old (car lat)) (cons new
                                             (cdr lat)))
                (else (cons (car lat) ((subst-f test?)
                                       new
                                       old
                                       (cdr lat))))))))
    (define insert-left
      (lambda (new old rest)
        (cons new
              (cons old rest))))
    (define insert-right
      (lambda (new old rest)
        (cons old
              (cons new rest))))
    (define insert-only-new
      (lambda (new old rest)
        (cons new rest)))
    (define insert-nothing
      (lambda (new old l)
        l))
    (define insert-gen
      (lambda (insert-func)
        (lambda (test?)
          (lambda (new old lat)
            (cond ((null? lat) '())
                  ((test? old (car lat)) (insert-func new
                                                      (car lat)
                                                      (cdr lat)))
                  (else (cons (car lat)
                              (((insert-gen insert-func) test?)
                               new
                               old
                               (cdr lat)))))))))))



