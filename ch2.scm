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

(define-library (sicp ex214)
  (export par1
          par2)
  (import (scheme base)
          (sicp ch214))
  (begin
    (define (par1 r1 r2)
      (/-interval
       (*-interval r1 r2)
       (+-interval r1 r2)))
    (define (par2 r1 r2)
      (let ((one (make-interval 1 1)))
        (/-interval one
                    (+-interval
                     (/-interval one r1)
                     (/-interval one r2)))))))

(define-library (sicp ch221)
  (export sicp-list-ref
          sicp-length
          sicp-append)
  (import (scheme base))
  (begin
    (define (sicp-list-ref items n)
      (cond ((= n 0) (car items))
            (else (sicp-list-ref (cdr items) (- n 1)))))
    (define (sicp-length items)
      (cond ((null? items) 0)
            (else (+ 1 (sicp-length (cdr items))))))
    (define (sicp-append list1 list2)
      (cond ((null? list1) list2)
            (else (cons (car list1)
                        (sicp-append (cdr list1) list2)))))))

(define-library (sicp ex217)
  (export ex217)
  (import (scheme base))
  (begin
    (define (ex217 items)
      (cond ((null? (cdr items)) items)
            (else (ex217 (cdr items)))))))

(define-library (sicp ex218)
  (export ex218)
  (import (scheme base)
          (sicp ch221))
  (begin
    (define (ex218 items)
      (define (helper items index)
        (cond ((< index 0) '())
              (else (cons (sicp-list-ref items index)
                          (helper items (- index 1))))))
      (helper items (- (sicp-length items) 1)))))

(define-library (sicp ex220)
  (export same-parity)
  (import (scheme base))
  (begin
    (define (same-parity first . rest)
      (define (find-same-parity items test)
        (cond ((null? items) '())
              ((test (car items)) (cons (car items)
                                        (find-same-parity (cdr items)
                                                          test)))
              (else (find-same-parity (cdr items) test))))
      (cond ((odd? first) (find-same-parity (cons first rest) odd?))
            (else (find-same-parity (cons first rest) even?))))))

(define-library (sicp ex221)
  (export ex221-no-map
          ex221-map)
  (import (scheme base))
  (begin
    (define (ex221-no-map items)
      (cond ((null? items) '())
            (else (cons (square (car items))
                        (ex221-no-map (cdr items))))))
    (define (ex221-map items)
      (map (lambda (x) (* x x)) items))))

(define-library (sicp ex222)
  (export ex222
          ex222-alt)
  (import (scheme base))
  (begin
    (define (ex222 items)
      (define (iter things answer)
        (cond ((null? things) answer)
              (else (iter (cdr things)
                          (cons (square (car things))
                                answer)))))
      (iter items '()))
    (define (ex222-alt items)
      (define (iter things answer)
        (cond ((null? things) answer)
              (else (iter (cdr things)
                          (cons answer
                                (square (car things)))))))
      (iter items '()))))
;;Exercise 2.22
;;In the first case, the list is reversed because cons always adds the
;;element to the front of the list, even though we are iterating through
;;the list from beginning to end.  Thus, the last item in the original
;;list will be added to the front of the answer list.
;;In the second case, we will get a dotted pair list because the last item
;;will not be the empty list, but instead will be the square of the last
;;item on the list.

(define-library (sicp ex223)
  (export ex223-for-each)
  (import (scheme base))
  (begin
    (define (ex223-for-each func items)
      (cond ((null? items) #t)
            (else (func (car items))
                  (ex223-for-each func (cdr items)))))))

(define-library (sicp ch222)
  (export count-leaves
          scale-tree
          scale-tree-map)
  (import (scheme base))
  (begin
    (define (count-leaves tree)
      (cond ((null? tree) 0)
            ((not (pair? tree)) 1)
            (else (+ (count-leaves (car tree))
                     (count-leaves (cdr tree))))))
    (define (scale-tree tree factor)
      (cond ((null? tree) '())
            ((not (pair? tree)) (* tree factor))
            (else (cons (scale-tree (car tree) factor)
                        (scale-tree (cdr tree) factor)))))
    (define (scale-tree-map tree factor)
      (map (lambda (sub-tree)
             (cond ((pair? sub-tree) (scale-tree-map sub-tree factor))
                   (else (* sub-tree factor))))
           tree))))

;;Exercise 2.24
;;(list 1 (list 2 (list 3 4))) will print (1  (2  (3  4)))
;;dotted pair notation:
;; [ ][]->[ ][/]
;;  |      |        
;;  1     [ ][]->[ ][/]  
;;         |      | 
;;         2     [ ][]->[ ][/]
;;                |      |
;;                3      4
;;Finally, in tree format:
;;             []
;;            /  \
;;           1   [] (2 (3 4))  
;;              /  \
;;             2   [] (3 4)
;;                /  \
;;               3   4

;;Exercise 2.25
;;(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;;(car (car ((7))))
;;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
;;    '(1 (2 (3 (4 (5 (6 7)))))))))))))))))

;;Exercise 2.26
; (append x y) -> (1 2 3 4 5 6)
; (cons x y) -> ((1 2 3) 4 5 6)
; (list x y) -> ((1 2 3) (4 5 6))

(define-library (sicp ex227)
  (export deep-reverse
          alt-reverse)
  (import (scheme base))
  (begin
    (define (deep-reverse items)
      (define (helper items reversed)
        (cond ((null? items) reversed)
              ((not (pair? items)) items)
              (else (helper (cdr items)
                            (cons (deep-reverse (car items))
                                  reversed)))))
      (helper items '()))
    (define (alt-reverse items)
      (define (helper items reversed)
        (cond ((null? items) reversed)
              (else (helper (cdr items) (cons (car items) reversed)))))
      (helper items '()))))

(define-library (sicp ex228)
  (export fringe)
  (import (scheme base))
  (begin
    (define (fringe tree)
      (cond ((null? tree) '())
            ((not (pair? tree)) (cons tree '()))
            (else (append (fringe (car tree))
                          (fringe (cdr tree))))))))

(define-library (sicp ex229)
  (export make-mobile
          mobile?
          make-branch
          mobile-weight
          mobile-balanced?)
  (import (scheme base))
  (begin
    (define (make-mobile left right)
      (list left right))
    (define (mobile? mobile)
      (list? mobile))
    (define (mobile-left-branch mobile)
      (car mobile))
    (define (mobile-right-branch mobile)
      (car (cdr mobile)))
    (define (make-branch length structure)
      (list length structure))
    (define (branch-length branch)
      (car branch))
    (define (branch-structure branch)
      (car (cdr branch)))
    (define (mobile-weight mobile)
      (cond ((not (mobile? mobile)) mobile)
            (else (+ (mobile-weight (branch-structure
                                     (mobile-left-branch mobile)))
                     (mobile-weight (branch-structure
                                     (mobile-right-branch mobile)))))))
    (define (mobile-balanced? mobile)
      (cond ((not (mobile? mobile)) #t)
            (else (and (mobile-balanced?
                        (branch-structure (mobile-left-branch mobile)))
                       (mobile-balanced?
                        (branch-structure (mobile-right-branch mobile)))
                       (= (* (branch-length (mobile-left-branch mobile))
                             (mobile-weight
                              (branch-structure (mobile-left-branch mobile))))
                          (* (branch-length (mobile-right-branch mobile))
                             (mobile-weight
                              (branch-structure (mobile-right-branch mobile)))))))))))

;;test data
;;        3      2
;;     ------[]------
;;     |            |
;;     4            6
;;(make-mobile (make-branch 3 4) (make-branch 2 5))
;;
;;            1        2
;;         -------[]-------
;;       3 | 4          5 |  6
;;    ---------        ------------
;;    |       |        |          |          
;;    7       8        9          10
;;(make-mobile (make-branch 1 (make-mobile (make-branch 3 7)
;;                                         (make-branch 4 8)))
;;             (make-branch 2 (make-mobile (make-branch 5 9)
;;                                         (make-branch 6 10))))


(define-library (sicp ex230)
  (export square-tree
          square-tree-map)
  (import (scheme base))
  (begin
    (define (square-tree tree)
      (cond ((null? tree) '())
            ((not (pair? tree)) (* tree tree))
            (else (cons (square-tree (car tree))
                        (square-tree (cdr tree))))))
    (define (square-tree-map tree)
      (map (lambda (sub-tree)
             (cond ((pair? sub-tree) (square-tree-map sub-tree))
                   (else (* sub-tree sub-tree))))
           tree))))

(define-library (sicp ex231)
  (export tree-map
          tree-map-square-tree)
  (import (scheme base))
  (begin
    (define (tree-map func tree)
      (map (lambda (sub-tree)
             (cond ((pair? sub-tree) (tree-map func sub-tree))
                   (else (func sub-tree))))
           tree))
    (define (tree-map-square-tree tree)
      (tree-map square tree))))

(define-library (sicp ex232)
  (export subsets)
  (import (scheme base))
  (begin
    (define (subsets s)
      (cond ((null? s) (list '()))
            (else (let ((rest (subsets (cdr s))))
                    (append rest (map (lambda (element) (cons (car s) element))
                                      rest))))))))
(define-library (sicp sequence-ops)
  (export accumulate
          filter
          enumerate-interval)
  (import (scheme base))
  (begin
    (define (accumulate op initial-value sequence)
      (cond ((null? sequence) initial-value)
            (else (op (car sequence)
                      (accumulate op initial-value (cdr sequence))))))
    (define (filter predicate sequence)
      (cond ((null? sequence) '())
            ((predicate (car sequence))
             (cons (car sequence) (filter predicate (cdr sequence))))
            (else (filter predicate (cdr sequence)))))
    (define (enumerate-interval low high step)
      (cond ((> low high) '())
            (else (cons low (enumerate-interval (step low) high step)))))))

(define-library (sicp ex233)
  (export accum-map
          accum-append
          accum-length)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (accum-map op sequence)
      (accumulate (lambda (x y) (cons (op x) y)) '() sequence))
    (define (accum-append seq1 seq2)
      (accumulate cons seq2 seq1))
    (define (accum-length sequence)
      (accumulate (lambda (x y) (+ 1 y)) 0 sequence))))

(define-library (sicp ex234)
  (export horner-eval)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (horner-eval x coefficient-sequence)
      (accumulate (lambda (this-coeff higher-terms)
                    (+ this-coeff (* x higher-terms)))
                  0
                  coefficient-sequence))))

(define-library (sicp ex235)
  (export count-leaves-accum)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (count-leaves-accum t)
      (accumulate (lambda (first previous-accums) 0) 0
                  (map (lambda (elem) elem) t)))))

(define-library (sicp ex236)
  (export accumulate-n)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (accumulate-n op init seqs)
      (cond ((null? (car seqs)) '())
            (else (cons (accumulate op init (map car seqs))
                        (accumulate-n op init (map cdr seqs))))))))

(define-library (sicp ex237)
  (export dot-product
          matrix-*-matrix
          matrix-*-vector
          transpose)
  (import (scheme base)
          (sicp sequence-ops)
          (sicp ex236))
  (begin
    (define (dot-product v w)
      (accumulate + 0 (map * v w)))
    (define (matrix-*-matrix m n)
      (let ((cols (transpose n)))
        (map (lambda (elem) elem) m)))
    (define (matrix-*-vector m v)
      (map (lambda (row) (accumulate + 0 (map * row v))) m))
    (define (transpose mat)
      (accumulate-n cons '() mat))))

(define-library (sicp ex238)
  (export fold-left)
  (import (scheme base))
  (begin
    (define (fold-left op initial sequence)
      (define (iter result rest)
        (cond ((null? rest) result)
              (else (iter (op result (car rest))
                          (cdr rest)))))
      (iter initial sequence))))
;;value of (fold-right / 1 (list 1 2 3)) => 3/2
;;value of (fold-left / 1 (list 1 2 3)) => 1/6
;;value of (fold-right list nil (list 1 2 3)) => (1 (2 (3 ())))
;;value of (fold-left list nil (list 1 2 3)) => (((() 1) 2) 3)

(define-library (sicp ex239)
  (export fold-right-reverse
          fold-left-reverse)
  (import (scheme base)
          (sicp ex238))
  (begin
    (define (fold-right-reverse sequence)
      (accumulate (lambda (x y) (cons y x)) '() sequence))
    (define (fold-left-reverse sequence)
      (fold-left (lambda (x y) (cons y x)) '() sequence))))
