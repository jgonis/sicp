(define-library (little-schemer)
  (export atom?)
  (import (scheme base)
          (scheme write))
  (begin
    (define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))))))

;;You can't ask for the car of a null list and you can't ask for
;;the cdr of a null list
