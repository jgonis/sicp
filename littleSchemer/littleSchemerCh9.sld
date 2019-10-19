(include "littleSchemerCh1.sld")
(include "littleSchemerCh4.sld")
(include "littleSchemerCh7.sld")
(define-library (little-schemer ch9)
  (export looking
          shift
          align
          atom-count)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch7))
  (begin
    (define keep-looking
      (lambda (a item lat)
        (cond ((number? item) (keep-looking a (pick item lat) lat))
              (else (equal? a item)))))
    (define looking
      (lambda (a lat)
        (keep-looking a (pick 1 lat) lat)))
    (define shift
      (lambda (pair)
        (build-pair (first (first pair))
                    (build-pair (second (first pair))
                                (second pair)))))
    (define align
      (lambda (pora)
        (cond ((atom? pora) pora)
              ((a-pair? pora) (align (shift pora)))
              (else (build-pair (first pora)
                                (align (second pora)))))))
    (define atom-count
      (lambda (pora)
        (cond ((null? pora) 0)
              ((atom? (car pora)) (add1 (atom-count (cdr pora))))
              (else (j+ (atom-count (car pora))
                        (atom-count (cdr pora)))))))
    (define shuffle
      (lambda (pora)
        (cond ((atom? pora) pora)
              ((a-pair? (first pora))
               (shuffle (revpair pora)))
              (else (build-pair (first pora)
                                (shuffle (second pora)))))))))
                                               


