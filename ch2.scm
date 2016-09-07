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
          x-point
          y-point
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

(define-library (sicp ex23-constructors)
  (export make-rectangle
          top-left-point
          bottom-right-point)
  (import (scheme base)
          (sicp ex22))
  (begin
    (define (make-rectangle top-left-point bottom-right-point)
      (cons top-left-point bottom-right-point))
    (define (top-left-point rectangle)
      (car rectangle))
    (define (bottom-right-point rectangle)
      (cdr rectangle))))

(define-library (sicp ex23-alt-constructors)
  (export make-rectangle
          top-left-point
          bottom-right-point)
  (import (scheme base)
          (sicp ex22))
  (begin
    (define (make-rectangle center-point width height)
      (cons center-point (cons width height)))
    (define (top-left-point rectangle)
      (let ((center-point (car rectangle))
            (width (car (cdr rectangle)))
            (height (cdr (cdr rectangle))))
        (make-point (- (x-point center-point) (/ width 2))
                    (- (y-point center-point) (/ height 2)))))
    (define (bottom-right-point rectangle)
      (let ((center-point (car rectangle))
            (width (car (cdr rectangle)))
            (height (cdr (cdr rectangle))))
        (make-point (+ (x-point center-point) (/ width 2))
                    (+ (y-point center-point) (/ height 2)))))))

(define-library (sicp ex23-functions)
  (export perimeter-of-rectangle
          area-of-rectangle
          print-rectangle)
  (import (scheme base)
          (scheme write)
          (sicp ex22)
          (sicp ex23-alt-constructors))
  (begin
    (define (print-rectangle rectangle)
      (display "Top Left: ")
      (print-point (top-left-point rectangle))
      (display " Bottom Right: ")
      (print-point (bottom-right-point rectangle)))
    (define (perimeter-of-rectangle rectangle)
      (+ (* 2 (abs (- (x-point (top-left-point rectangle))
                      (x-point (bottom-right-point rectangle)))))
         (* 2 (abs (- (y-point (top-left-point rectangle))
                      (y-point (bottom-right-point rectangle)))))))
    (define (area-of-rectangle rectangle)
      (* (abs (- (x-point (top-left-point rectangle))
                 (x-point (bottom-right-point rectangle))))
         (abs (- (y-point (top-left-point rectangle))
                 (y-point (bottom-right-point rectangle))))))))
(define-library (sicp ex24)
  (export ex24-cons
          ex24-car
          ex24-cdr)
  (import (scheme base))
  (begin
    (define (ex24-cons x y)
      (lambda (m) (m x y)))
    (define (ex24-car z)
      (z (lambda (p q) p)))
    (define (ex24-cdr z)
      (z (lambda (p q) q)))))

(define-library (sicp ex25)
  (export ex25-cons
          ex25-car
          ex25-cdr)
  (import (scheme base))
  (begin
    (define (ex25-cons x y) 1)
    (define (ex25-car z) 1)
    (define (ex25-cdr z) 1)))
