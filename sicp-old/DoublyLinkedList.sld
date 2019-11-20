(define-library (sicp doubly-linked-list)
  (export make-dlist
          dlist-next
          dlist-previous
          dlist-set-next!
          dlist-set-previous!
          dlist-item
          dlist-set-item!
          dlist->list
          dlist-print)
  (import (scheme base)
          (scheme write))
  (begin
    (define (make-dlist item next previous)
      (cons item (cons next previous)))
    (define (dlist-next dlist)
      (cond ((null? dlist) (error "Expected a dlist, got ()"))
            (else (car (cdr dlist)))))
    (define (dlist-previous dlist)
      (cond ((null? dlist) (error "Expected a dlist, got ()"))
            (else (cdr (cdr dlist)))))
    (define (dlist-set-next! dlist new-next)
      (set-car! (cdr dlist) new-next))
    (define (dlist-set-previous! dlist new-previous)
      (set-cdr! (cdr dlist) new-previous))
    (define (dlist-item dlist)
      (car dlist))
    (define (dlist->list dlist)
      (cond ((null? (dlist-next dlist)) (cons (dlist-item dlist) '()))
            (else (cons (dlist-item dlist) (dlist->list (dlist-next dlist))))))
    (define (dlist-set-item! dlist new-item)
      (set-car! dlist new-item))))
