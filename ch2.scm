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
    (define (ex25-cons x y)
      (* (expt 2 x) (expt 3 y)))
    (define (ex25-car z)
      (define (helper z count)
        (cond ((even? z) (helper (/ z 2) (+ count 1)))
              (else count)))
      (helper z 0))
    (define (ex25-cdr z)
      (define (helper z count)
        (cond ((= z 1) count)
              (else (helper (/ z 3) (+ count 1)))))
      (let ((car (expt 2 (ex25-car z))))
        (helper (/ z car) 0)))))

(define-library (sicp ch214)
  (export make-interval
          make-interval-from-center
          make-interval-from-percent
          lower-bound
          upper-bound
          interval-center
          interval-percent-tolerance
          print-interval
          +-interval
          *-interval
          /-interval
          subtract-interval
          interval-width)
  (import (scheme base)
          (scheme write))
  (begin
    (define (make-interval lower-bound upper-bound)
      (cons lower-bound upper-bound))
    (define (make-interval-from-center center width)
      (make-interval (- center width) (+ center width)))
    (define (make-interval-from-percent center percent-tolerance)
      (make-interval (- center (* center percent-tolerance))
                     (+ center (* center percent-tolerance))))
    (define (interval-percent-tolerance interval)
      1)
    (define (interval-center interval)
      (/ (+ (lower-bound interval)
            (upper-bound interval))
         2))
    (define (interval-width interval)
      (/ (- (upper-bound interval)
            (lower-bound interval))
         2))
    (define (lower-bound interval)
      (car interval))
    (define (upper-bound interval)
      (cdr interval))
    (define (print-interval interval)
      (display "Lower bound: ")
      (display (lower-bound interval))
      (display " Upper bound: ")
      (display (upper-bound interval)))
    (define (+-interval i1 i2)
      (make-interval (+ (lower-bound i1)
                        (lower-bound i2))
                     (+ (upper-bound i1)
                        (upper-bound i2))))
    (define (*-interval i1 i2)
      (let ((p1 (* (lower-bound i1)
                   (lower-bound i2)))
            (p2 (* (lower-bound i1)
                   (upper-bound i2)))
            (p3 (* (upper-bound i1)
                   (lower-bound i2)))
            (p4 (* (upper-bound i1)
                   (upper-bound i2))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))
    (define (/-interval i1 i2)
      (cond ((= 0 (upper-bound i2))
             (error "interval contains 0 and cannot be divided"
                    (upper-bound i2)))
            ((= 0 (lower-bound i2))
             (error "interval contains 0 and cannot be divided"
                    (lower-bound i2)))
            (else (*-interval i1
                              (make-interval (/ 1 (upper-bound i2))
                                             (/ 1 (lower-bound i2)))))))
    (define (subtract-interval i1 i2)
      (let ((p1 (- (lower-bound i1)
                   (lower-bound i2)))
            (p2 (- (lower-bound i1)
                   (upper-bound i2)))
            (p3 (- (upper-bound i1)
                   (lower-bound i2)))
            (p4 (- (upper-bound i1)
                   (upper-bound i2))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))))
