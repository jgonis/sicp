(include "ch1.scm")

(define-library (sicp ch211)
  (export make-rational-number
          add-rational-numbers
          subtract-rational-numbers
          multiply-rational-numbers
          divide-rational-numbers
          rational-numbers-equal?
          print-rational-number)
  (import (scheme base)
          (scheme write))
  (begin
    (define (add-rational-numbers x y)
      (make-rational-number (+ (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
    (define (subtract-rational-numbers x y)
      (make-rational-number (- (* (numer x) (denom y))
                               (* (numer y) (denom x)))
                            (* (denom x) (denom y))))
    (define (multiply-rational-numbers x y)
      (make-rational-number (* (numer x) (numer y))
                            (* (denom x) (denom y))))
    (define (divide-rational-numbers x y)
      (make-rational-number (* (numer x) (denom y))
                            (* (denom x) (numer y))))
    (define (rational-numbers-equal? x y)
      (= (* (numer x) (denom y))
         (* (numer y) (denom x))))
    (define (make-rational-number numer denom)
      (let ((g (gcd numer denom)))
        (cond ((and (negative? numer) (negative? denom))
               (cons (/ (* -1 numer) g)
                     (/ (* -1 denom) g)))
              ((and (positive? numer) (negative? denom))
               (cons (/ (* -1 numer) g)
                     (/ (* -1 denom) g)))
              (else (cons (/ numer g) (/ denom g))))))
    (define (numer rational-number)
      (car rational-number))
    (define (denom rational-number)
      (cdr rational-number))
    (define (print-rational-number rational-number)
      (display (numer rational-number))
      (display "/")
      (display (denom rational-number)))))

(define-library (sicp ex22)
  (export make-segment
          make-point
          midpoint-segment
          print-point
          print-segment)
  (import (scheme base)
          (scheme write)
          (sicp math-funcs))
  (begin
    (define (make-segment start-point end-point)
      (cons start-point end-point))
    (define (start-segment line-segment)
      (car line-segment))
    (define (end-segment line-segment)
      (cdr line-segment))
    (define (print-segment line-segment)
      (print-point (start-segment line-segment))
      (display " --> ")
      (print-point (end-segment line-segment)))
    (define (midpoint-segment line-segment)
      (make-point (average (x-point (start-segment line-segment))
                           (x-point (end-segment line-segment)))
                  (average (y-point (start-segment line-segment))
                           (y-point (end-segment line-segment)))))
    (define (make-point x y)
      (cons x y))
    (define (x-point point)
      (car point))
    (define (y-point point)
      (cdr point))
    (define (print-point point)
      (display "(")
      (display (x-point point))
      (display ", ")
      (display (y-point point))
      (display ")"))))

(define-library (sicp ex23)
  (export make-rectangle
          print-rectangle
          perimeter-of-rectangle
          area-of-rectangle)
  (import (scheme base)
          (scheme write)
          (sicp ex22))
  (begin
    (define (make-rectangle) 1)
    (define (print-rectangle) 1)
    (define (perimeter-of-rectangle) 1)
    (define (area-of-rectangle) 1)))
