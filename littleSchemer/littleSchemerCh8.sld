(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
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
          insert-gen
          multirember-f
          multiremberT
          multirember&co
          multiinsertLR
          multiinsertLR&co
          evens-only*
          evens-only*&co)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4))
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
                               (cdr lat)))))))))
    (define multirember-f
      (lambda (test?)
        (lambda (a lat)
          (cond ((null? lat) '())
                ((test? a (car lat)) ((multirember-f test?)
                                      a
                                      (cdr lat)))
                (else (cons (car lat) ((multirember-f test?)
                                       a
                                       (cdr lat))))))))
    (define multiremberT
      (lambda (test-func lat)
        (cond ((null? lat) '())
              ((test-func (car lat)) (multiremberT test-func
                                                   (cdr lat)))
              (else (cons (car lat)
                          (multiremberT test-func
                                        (cdr lat)))))))
    (define multirember&co
      (lambda (a lat col)
        (cond ((null? lat) (col (quote ()) (quote ())))
              ((eq? (car lat) a)
               (multirember&co a
                               (cdr lat)
                               (lambda (newlat seen)
                                 (col newlat
                                      (cons (car lat) seen)))))
              (else (multirember&co a
                                    (cdr lat)
                                    (lambda (newlat seen)
                                      (col (cons (car lat) newlat)
                                           seen)))))))
    (define multiinsertLR
      (lambda (new oldL oldR lat)
        (cond ((null? lat) '())
              ((eq? oldL (car lat))
               (cons new
                     (cons oldL
                           (multiinsertLR new oldL oldR (cdr lat)))))
              ((eq? oldR (car lat))
               (cons oldR
                     (cons new
                           (multiinsertLR new oldL oldR (cdr lat)))))
              (else (cons (car lat)
                          (multiinsertLR new oldL oldR (cdr lat)))))))
    (define multiinsertLR&co
      (lambda (new oldL oldR lat col)
        (cond ((null? lat) (col (quote ()) 0 0))
              ((eq? oldL (car lat))
               (multiinsertLR&co new
                                 oldL
                                 oldR
                                 (cdr lat)
                                 (lambda (newlat l-count r-count)
                                   (col (cons new
                                              (cons (car lat)
                                                    newlat))
                                        (add1 l-count)
                                        r-count))))
              ((eq? oldR (car lat))
               (multiinsertLR&co new
                                 oldL
                                 oldR
                                 (cdr lat)
                                 (lambda (newlat l-count r-count)
                                   (col (cons (car lat)
                                              (cons new
                                                    newlat))
                                        l-count
                                        (add1 r-count)))))
              (else (multiinsertLR&co new
                                      oldL
                                      oldR
                                      (cdr lat)
                                      (lambda (newlat l-count r-count)
                                        (col (cons (car lat)
                                                   newlat)
                                             l-count
                                             r-count)))))))
    (define evens-only*
      (lambda (l)
        (cond ((null? l) '())
              ((and (atom? (car l)) (j-even? (car l)))
               (cons (car l) (evens-only* (cdr l))))
              ((atom? (car l)) (evens-only* (cdr l)))
              (else (cons (evens-only* (car l))
                          (evens-only* (cdr l)))))))
    (define evens-only*&co
      (lambda (l col)
        (cond ((null? l) (col '() 1 0))
              ((and (atom? (car l)) (j-even? (car l)))
               (evens-only*&co (cdr l)
                               (lambda (evens even-prod odd-sum)
                                 (col (cons (car l) evens)
                                      (j* (car l) even-prod)
                                      odd-sum))))
              ((atom? (car l))
               (evens-only*&co (cdr l)
                               (lambda (evens even-prod odd-sum)
                                 (col evens
                                      even-prod
                                      (j+ (car l) odd-sum)))))
              (else (evens-only*&co
                     (car l)
                     (lambda (car-evens car-prod car-sum)
                       (evens-only*&co
                        (cdr l)
                        (lambda (cdr-evens cdr-prod cdr-sum)
                          (col (cons car-evens cdr-evens)
                               (j* car-prod cdr-prod)
                               (j+ car-sum cdr-sum))))))))))))

;;first attempt at a naive solution
              ;; (else (col (cons (evens-only*&co (car l)
              ;;                                  (lambda (evens e-p o-s)
              ;;                                    evens))
              ;;                  (evens-only*&co (cdr l)
              ;;                                  (lambda (evens e-p o-s)
              ;;                                    evens)))
              ;;            (j* (evens-only*&co (car l)
              ;;                                (lambda (evens e-p o-s)
              ;;                                  e-p))
              ;;                (evens-only*&co (cdr l)
              ;;                                (lambda (evens e-p o-s)
              ;;                                  e-p)))
              ;;            (j+ (evens-only*&co (car l)
              ;;                                (lambda (evens e-p o-s)
              ;;                                  o-s))
              ;;                (evens-only*&co (cdr l)
              ;;                                (lambda (evens e-p o-s)
              ;;                                  o-s))))))))))
                                               


