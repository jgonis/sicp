(include "ch1.scm")
(include "utils.scm")
(include "tree.sld")

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
                              (make-interval
                               (/ 1 (upper-bound i2))
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
;;element to the front of the list, even though we are iterating
                                        ;through
;;the list from beginning to end.  Thus, the last item in the original
;;list will be added to the front of the answer list.
;;In the second case, we will get a dotted pair list because the last
;;item will not be the empty list, but instead will be the square of
;;the last item on the list.

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
;; (append x y) -> (1 2 3 4 5 6)
;; (cons x y) -> ((1 2 3) 4 5 6)
;; (list x y) -> ((1 2 3) (4 5 6))

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
              (else (helper (cdr items)
                            (cons (car items) reversed)))))
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
            (else (+ (mobile-weight
                      (branch-structure
                       (mobile-left-branch mobile)))
                     (mobile-weight
                      (branch-structure
                       (mobile-right-branch mobile)))))))
    (define (mobile-balanced? mobile)
      (cond ((not (mobile? mobile)) #t)
            (else (and (mobile-balanced?
                        (branch-structure
                         (mobile-left-branch mobile)))
                       (mobile-balanced?
                        (branch-structure
                         (mobile-right-branch mobile)))
                       (= (* (branch-length
                              (mobile-left-branch mobile))
                             (mobile-weight
                              (branch-structure
                               (mobile-left-branch mobile))))
                          (* (branch-length
                              (mobile-right-branch mobile))
                             (mobile-weight
                              (branch-structure
                               (mobile-right-branch mobile)))))))))))

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
                    (append rest (map (lambda (element)
                                        (cons (car s) element))
                                      rest))))))))
;;The following is the for the example argument of (1 2 3):
;;The subsets function works by iterating to the end of the list
;;and then return a list with an empty list inside of it. From there
;;the recursion continues back through the list, starting with (3),
;;proceeding to (2 3) and ending back at the original (1 2 3). At
;;each step the first element of this list is inserted into each
;;element
;;of a copy of the current result list which is then appended to back
;;of the current result list, creating a new result list that we
;;continue to recurse with.
;;Example we get all the way to the end of the list and return (()).
;;We now recur back to the time when our argument list is (3). We
;;insert
;;3 into a copy of the result list giving us ((3)) and append that to
;;the result list giving (() (3)). We recur again, with our argument
;;list now being (2 3). We insert 2 into a copy of the result list
;;giving ((2) (2 3)) and append this to the result list giving
;;(() (3) (2) (2 3)). We recur once again and repeat this procedure.

(define-library (sicp sequence-ops)
  (export accumulate
          filter
          enumerate-interval
          flatmap)
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
    (define (enumerate-interval low high . step)
      (let ((stepFunc (cond ((null? step) (lambda (x) (+ x 1)))
                            (else (car step)))))
        (cond ((> low high) '())
              (else (cons low (enumerate-interval (stepFunc low)
                                                  high
                                                  stepFunc))))))
    (define (flatmap func sequence)
      (accumulate append '() (map func sequence)))))

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
        (map (lambda (row)
               (map (lambda (column) (dot-product row column))
                    cols))
             m)))
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
          (sicp ex238)
          (sicp sequence-ops))
  (begin
    (define (fold-right-reverse sequence)
      (accumulate (lambda (x y) (cons y x)) '() sequence))
    (define (fold-left-reverse sequence)
      (fold-left (lambda (x y) (cons y x)) '() sequence))))

(define-library (sicp nested-mappings)
  (export prime-sum-pairs)
  (import (scheme base)
          (sicp fast-prime)
          (sicp sequence-ops))
  (begin
    (define (make-pairs n)
      (accumulate append '()
                  (map (lambda (right)
                         (map (lambda (left) (cons left right))
                              (enumerate-interval 1 right)))
                       (enumerate-interval 1 n))))
    (define (prime-sum? pair)
      (prime? (+ (car pair) (cdr pair))))
    (define (prime-sum-pairs n)
      (map (lambda (prime-pair) (cons (+ (car prime-pair)
                                         (cdr prime-pair))
                                      prime-pair))
           (filter prime-sum? (make-pairs n))))))

(define-library (sicp permutations)
  (export permutations)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (remove item sequence)
      (filter (lambda (x) (not (= x item)))
              sequence))
    (define (permutations s)
      (cond ((null? s) (list '()))
            (else (flatmap (lambda (x)
                             (map (lambda (p)
                                    (cons x p))
                                  (permutations
                                   (remove x s))))
                           s))))))

(define-library (sicp ex241)
  (export ex241)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (generate-triplets n)
      (accumulate append
                  '()
                  (accumulate
                   append
                   '()
                   (map (lambda (left)
                          (map (lambda (middle)
                                 (map (lambda (right)
                                        (list left middle right))
                                      (enumerate-interval 1
                                                          middle)))
                               (enumerate-interval 1
                                                   left)))
                        (enumerate-interval 1
                                            n)))))
    (define (filter-triplets pred? triplets)
      (cond ((null? triplets) '())
            ((pred? (car triplets))
             (cons (car triplets)
                   (filter-triplets pred? (cdr triplets))))
            (else (filter-triplets pred? (cdr triplets)))))
    (define (unique-triplet? triplet)
      (let ((a (car triplet))
            (b (car (cdr triplet)))
            (c (car (cdr (cdr triplet)))))
        (and (not (= a b))
             (not (= a c))
             (not (= b c)))))
    (define (triplet-sum triplet)
      (let ((a (car triplet))
            (b (car (cdr triplet)))
            (c (car (cdr (cdr triplet)))))
        (+ a b c)))
    (define (ex241 n s)
      (let ((triplets (filter-triplets unique-triplet?
                                       (generate-triplets n))))
        (filter-triplets (lambda (triplet)
                           (= (triplet-sum triplet) s))
                         triplets)))))

(define-library (sicp ex242)
  (export ex242)
  (import (scheme base)
          (sicp sequence-ops))
  (begin
    (define (board-valid board)
      (define (helper position rest-of-board)
        (cond ((null? rest-of-board) #t)
              (else (and (not (= (car (car rest-of-board))
                                 position))
                         (helper position
                                 (cdr rest-of-board))))))
      (cond ((null? board) #t)
            (else (and (helper (car (car board)) (cdr board))
                       (board-valid (cdr board))))))
    (define (ex242 board-size)
      (define (helper current-column current-board board-size)
        (cond ((= current-column 1)
               (filter (lambda (board) (board-valid board))
                       (map (lambda (current-position)
                              (cons (cons current-position
                                          current-column)
                                    current-board))
                            (enumerate-interval 1 board-size))))
              (else (filter (lambda (board) (not (null? board)))
                            (flatmap
                             (lambda (current-position)
                               (helper (- current-column 1)
                                       (cons
                                        (cons current-position
                                              current-column)
                                        current-board)
                                       board-size))
                             (enumerate-interval 1
                                                 board-size))))))
      (helper board-size '() board-size))))

(define-library (sicp ex242-alt)
  (export ex242-alt)
  (import (scheme base)
          (sicp sequence-ops)
          (scheme write))
  (begin
    (define empty-board '())
    (define (ex242-alt board-size)
      (define (queen-cols current-column)
        (cond ((= current-column 0) (list empty-board))
              (else (filter
                     (lambda (positions)
                       (safe? current-column positions))
                     (flatmap
                      (lambda (rest-of-queens)
                        (map (lambda (row-position)
                               (adjoin-position row-position
                                                current-column
                                                rest-of-queens))
                             (enumerate-interval 1 board-size)))
                      (queen-cols (- current-column 1)))))))
      (queen-cols board-size))
    (define (safe? column positions)
      (define (helper position column current)
        (cond ((= current 0) #t)))
      (display column))
    (define (adjoin-position row-position column rest-of-queens)
      (cons (cons row-position column) rest-of-queens))))


;;;;;;;;;;;;;;;;;;;;;;;;  Section 2.3  ;;;;;;;;;;;;;;;;;;;;;;;
(define-library (sicp ch231)
  (export my-equal?)
  (import (scheme base))
  (begin
    (define (my-equal? x y)
      (cond ((and (not (pair? x)) (not (pair? y))) (eq? x y))
            ((and (not (pair? x)) (pair? y)) #f)
            ((and (pair? x) (not (pair? y))) #f)
            ((and (null? x) (null? y)) #t)
            ((or (null? x) (null? y)) #f)
            (else (and (my-equal? (car x) (car y))
                       (my-equal? (cdr x) (cdr y))))))))

(define-library (sicp ch232)
  (export deriv)
  (import (scheme base))
  (begin
    (define (variable? x) (symbol? x))
    (define (same-variable? x y)
      (and (variable? x)
           (variable? y)
           (eq? x y)))
    (define (sum? x)
      (and (pair? x) (eq? (car x) '+)))
    (define (addend sum) (car (cdr sum)))
    (define (augend sum) (car (cdr (cdr sum))))
    (define (make-sum a1 a2)
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list '+ a1 a2))))
    (define (=number? exp num)
      (and (number? exp) (= exp num)))
    (define (product? x)
      (and (pair? x) (eq? (car x) '*)))
    (define (multiplier product) (car (cdr product)))
    (define (multiplicand product) (car (cdr (cdr product))))
    (define (make-product m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list '* m1 m2))))
    (define (deriv exp var)
      (cond ((number? exp) 0)
            ((variable? exp)
             (cond ((same-variable? exp var) 1)
                   (else 0)))
            ((sum? exp)
             (make-sum (deriv (addend exp) var)
                       (deriv (augend exp) var)))
            ((product? exp)
             (make-sum
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))
            (else (error "unknown expression type!"))))))

(define-library (sicp unordered-list-sets)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set)
  (import (scheme base))
  (begin
    (define (union-set set1 set2)
      (cond ((null? set1) set2)
            ((element-of-set? (car set1) set2)
             (union-set (cdr set1) set2))
            (else (union-set (cdr set1) (cons (car set1) set2)))))
    (define (intersection-set set1 set2)
      (cond ((or (null? set1) (null? set2)) '())
            ((element-of-set? (car set1) set2)
             (cons (car set1) (intersection-set (cdr set1) set2)))
            (else (intersection-set (cdr set1) set1))))
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((equal? x (car set)) #t)
            (else (element-of-set? x (cdr set)))))
    (define (adjoin-set x set)
      (cond ((element-of-set? x set) set)
            (else (cons x set))))))
;;Exercise 2.60
;;Allowing duplicates doesn't change the worst case run-time for
;;element-of-set?, but it could change the constant factors
;;involved as you may have to pass over multiple duplicates
;;Adjoin-set becomes much quicker as you don't need to check for
;;duplicates and can just append the item instantly for constant
;;run-time. Union-set also becomes much quicker as you also do not
;;need to search for duplicates and can instead just append the sets
;;together. Intersection-set remains n^2, but the constant factors
;;will grow as you may be iterating over a large number of duplicates

(define-library (sicp ordered-list-sets)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set)
  (import (scheme base))
  (begin
    (define (union-set set1 set2)
      (cond ((null? set1) set2)
            ((null? set2) set1)
            (else
             (let ((x1 (car set1))
                   (x2 (car set2)))
               (cond ((< x1 x2) (cons x1 (union-set (cdr set1)
                                                    set2)))
                     ((> x1 x2) (cons x2 (union-set set1
                                                    (cdr set2))))
                     (else (cons x1 (union-set (cdr set1)
                                               (cdr set2)))))))))
    (define (intersection-set set1 set2)
      (cond ((or (null? set1) (null? set2)) '())
            (else
             (let ((x1 (car set1))
                   (x2 (car set2)))
               (cond ((= x1 x2) (cons x1 (intersection-set
                                          (cdr set1)
                                          (cdr set2))))
                     ((< x1 x2) (intersection-set (cdr set1)
                                                  set2))
                     ((> x1 x2) (intersection-set set1
                                                  (cdr set2))))))))
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((= x (car set)) #t)
            ((< x (car set)) #f)
            (else (element-of-set? x (cdr set)))))
    (define (adjoin-set x set)
      (cond ((null? set) (cons x '()))
            ((< x (car set)) (cons x set))
            (else (cons (car set) (adjoin-set x (cdr set))))))))

(define-library (sicp binary-tree-sets)
  (export union-set
          intersection-set
          element-of-set?
          adjoin-set
          tree->list-1
          tree->list-2
          list->tree
          partial-tree)
  (import (scheme base)
          (sicp tree-lib)
          (prefix (sicp ordered-list-sets) ol-))
  (begin
    (define (union-set set1 set2)
      (list->tree (ol-union-set (tree->list-2 set1)
                                (tree->list-2 set2))))
    (define (intersection-set set1 set2)
      (list->tree (ol-intersection-set (tree->list-2 set1)
                                       (tree->list-2 set2))))
    (define (element-of-set? x set)
      (cond ((null? set) #f)
            ((= x (node set)) #t)
            ((< x (node set)) (element-of-set? x
                                               (left-branch set)))
            ((> x (node set)) (element-of-set? x
                                               (right-branch set)))))
    (define (adjoin-set x set)
      (cond ((null? set) (make-tree x '() '()))
            ((= x (node set)) set)
            ((< x (node set))
             (make-tree (node set)
                        (adjoin-set x
                                    (left-branch set))
                        (right-branch set)))
            ((> x (node set))
             (make-tree (node set)
                        (left-branch set)
                        (adjoin-set x
                                    (right-branch set))))))
    (define (tree->list-1 tree)
      (cond ((null? tree) '())
            (else (append (tree->list-1 (left-branch tree))
                          (cons (node tree)
                                (tree->list-1 (right-branch tree)))))))
    
    (define (tree->list-2 tree)
      (define (copy-to-list tree result-list)
        (cond ((null? tree)
               result-list)
              (else
               (copy-to-list (left-branch tree)
                             (cons (node tree)
                                   (copy-to-list (right-branch tree)
                                                 result-list))))))
      (copy-to-list tree '()))
    ;;Exercise 2.63 - Both methods of converting a tree to a list
    ;;convert to the exact same list. The second method will run faster
    ;;because it is using cons instead of append.  Append runs in
    ;;linear time while cons runs in constant time.
    
    (define (list->tree lyst)
      (car (partial-tree lyst
                         (length lyst))))
    ;;Exercise 2.64
    ;;partial-tree works by recursively dividing the list "in half",
    ;;and then calling partial-tree again once for the left tree and
    ;;once for the right tree. Once the list size we are sub-dividing
    ;;is down to one we create a tree with no children and return that
    ;;which in turn becomes the left or right branch of the parent node
    ;;and so on back up until we hit the (roughly) mid-point of the
    ;;list, which is our root node. Because we only visit each list
    ;;element once as we traverse through the list the runtime is O(n).
    (define (partial-tree elements n)
      (cond ((= n 0) (cons '() elements))
            (else (let* ((left-size (quotient (- n 1) 2))
                         (left-result (partial-tree elements
                                                    left-size))
                         (left-tree (car left-result))
                         (non-left-elements (cdr left-result))
                         (right-size (- n (+ left-size 1)))
                         (this-entry (car non-left-elements))
                         (right-result (partial-tree
                                        (cdr non-left-elements)
                                        right-size))
                         (right-tree (car right-result))
                         (remaining-elements (cdr right-result)))
                    (cons (make-tree this-entry
                                     left-tree
                                     right-tree)
                          remaining-elements)))))))

(define-library (sicp ex266)
  (export ex266-lookup)
  (import (scheme base)
          (sicp tree-lib))
  (begin
    (define (key records) '())
    (define (ex266-lookup given-key set-of-records)
      (cond ((null? set-of-records) #f)
            ((equal? given-key (key (node set-of-records)))
             (node set-of-records))
            ((< given-key (key (node set-of-records)))
             (ex266-lookup given-key
                           (left-branch set-of-records)))
            ((> given-key (key (node set-of-records)))
             (ex266-lookup given-key
                           (right-branch set-of-records)))))))

(define-library (sicp huffman-base)
  (export make-leaf
          leaf?
          leaf-symbol
          leaf-weight
          left-tree
          right-tree
          tree-symbols
          tree-weight)
  (import (scheme base)
          (scheme cxr))
  (begin
    (define (make-leaf symbol weight)
      (list 'leaf symbol weight))
    (define (leaf? node)
      (eq? (car node) 'leaf))
    (define (leaf-symbol leaf)
      (cadr leaf))
    (define (leaf-weight leaf)
      (caddr leaf))
    (define (left-tree tree)
      (car tree))
    (define (right-tree tree)
      (cadr tree))
    (define (tree-symbols tree)
      (cond ((leaf? tree) (list (leaf-symbol tree)))
            (else (caddr tree))))
    (define (tree-weight tree)
      (cond ((leaf? tree) (leaf-weight tree))
            (else (cadddr tree))))))

(define-library (sicp huffman-encoding)
  (export make-code-tree
          adjoin-set
          make-leaf-set
          encode)
  (import (scheme base)
          (sicp huffman-base))
  (begin
    (define (make-code-tree left-tree right-tree)
      (list left-tree
            right-tree
            (append (tree-symbols left-tree)
                    (tree-symbols right-tree))
            (+ (tree-weight left-tree)
               (tree-weight right-tree))))
    (define (adjoin-set x set)
      (cond ((null? set) (list x))
            ((< (tree-weight x) (tree-weight (car set)))
             (cons x set))
            (else (cons (car set) (adjoin-set x (cdr set))))))
    
    (define (make-leaf-set pairs)
      (cond ((null? pairs) '())
            (else (let ((pair (car pairs)))
                    (adjoin-set (make-leaf (car pair)
                                                   (cadr pair))
                                        (make-leaf-set (cdr pairs)))))))
    (define (encode message tree)
      (cond ((null? message) '())
            (else (append (encode-symbol (car message)
                                         tree)
                          (encode (cdr message)
                                  tree)))))
    (define (encode-symbol symbol tree)
      (define (go-left? symbol symbol-list)
        (cond ((null? symbol-list) #f)
              ((eq? symbol (car symbol-list)) #t)
              (else (go-left? symbol (cdr symbol-list)))))
      (cond ((leaf? tree) (cond ((eq? (leaf-symbol tree) symbol) '())
                                (else (error "unknown symbol!"))))
            ((go-left? symbol (tree-symbols (left-tree tree)))
             (cons 0 (encode-symbol symbol (left-tree tree))))
            (else (cons 1 (encode-symbol symbol (right-tree tree))))))))

(define-library (sicp huffman-decoding)
  (export decode)
  (import (scheme base)
          (sicp huffman-base))
  (begin
    (define (decode bits tree)
      (define (choose-branch bit tree)
        (cond ((= bit 0) (left-tree tree))
              ((= bit 1) (right-tree tree))
              (else (error "bad bit!"))))
      (define (decode-1 bits current-branch)
        (cond ((null? bits) '())
              (else (let ((next-branch (choose-branch (car bits)
                                                      current-branch)))
                      (cond ((leaf? next-branch)
                             (cons (leaf-symbol next-branch)
                                   (decode-1 (cdr bits) tree)))
                            (else (decode-1 (cdr bits)
                                            next-branch)))))))
      (decode-1 bits tree))))

(define-library (sicp ex267)
  (export ex267
          sample-tree
          sample-message)
  (import (scheme base)
          (sicp huffman-base)
          (sicp huffman-encoding)
          (sicp huffman-decoding))
  (begin
    (define sample-tree
      (make-code-tree
       (make-leaf 'A 4)
       (make-code-tree
        (make-leaf 'B 2)
        (make-code-tree
         (make-leaf 'D 1)
         (make-leaf 'C 1)))))
    (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
    (define (ex267)
      (decode sample-message sample-tree))))

(define-library (sicp ex268)
  (export ex268)
  (import (scheme base)
          (sicp ex267)
          (sicp huffman-encoding)
          (sicp huffman-decoding)
          (scheme write))
  (begin
    (define (ex268)
      (let ((message (decode sample-message sample-tree)))
        (encode message sample-tree)))))

(define-library (sicp ex269)
  (export generate-huffman-tree)
  (import (scheme base)
          (sicp huffman-encoding)
          (sicp huffman-base))
  (begin
    (define (generate-huffman-tree pairs)
      (define (successive-merge pairs)
        (cond ((= (length pairs) 1) (car pairs))
              (else (let ((first (car pairs))
                          (second (cadr pairs))
                          (rest (cddr pairs)))
                      (successive-merge (adjoin-set
                                         (make-code-tree first
                                                         second)
                                         rest))))))
      (successive-merge
       (make-leaf-set pairs)))))


(define-library (sicp ex270)
  (export ex270
          ex270-alphabet
          ex270-message)
  (import (scheme base)
          (sicp huffman-encoding)
          (sicp ex269))
  (begin
    (define (ex270)
      (encode ex270-message (generate-huffman-tree ex270-alphabet)))
    (define ex270-alphabet
      '((A 2) (BOOM 1) (GET 2) (JOB 2)
        (NA 16) (SHA 3) (YIP 9) (WAH 1)))
    (define ex270-message '(GET A JOB SHA NA NA NA NA NA NA NA NA
                                GET A JOB SHA NA NA NA NA NA NA NA
                                NA WAH YIP YIP YIP YIP YIP YIP YIP
                                YIP YIP SHA BOOM))))

;;Ex2.70
;;Encoding the song with huffman codes requires 84bits, while using a fixed
;;length code (3 bits ber alphabet letter as there at 8 letters) multiplied
;;by the 36 "letters" used is 108 bits for fixed-length coding.

(define-library (sicp type-tags)
  (export attach-tag
          type-tag
          contents)
  (import (scheme base))
  (begin
    (define (attach-tag type-tag contents)
      (cons type-tag contents))
    (define (type-tag datum)
      (cond ((pair? datum) (car datum))
            (else (error "Bad tagged datum: TYPE-TAG" datum))))
    (define (contents datum)
      (cond ((pair? datum) (cdr datum))
            (else (error "Bad tagged datum: CONTENTS" datum))))))

(define-library (sicp complex-num-lib-rectangular)
  (export real-part-rectangular
          imag-part-rectangular
          magnitude-rectangular
          angle-rectangular
          make-from-real-imag-rectangular
          make-from-mag-angle-rectangular)
  (import (scheme base)
          (scheme inexact)
          (sicp type-tags))
  (begin
    (define (real-part-rectangular z)
      (car z))
    (define (imag-part-rectangular z)
      (cdr z))
    (define (magnitude-rectangular z)
      (sqrt (+ (square (real-part-rectangular z))
               (square (imag-part-rectangular z)))))
    (define (angle-rectangular z)
      (atan (imag-part-rectangular z)
            (real-part-rectangular z)))
    (define (make-from-real-imag-rectangular x y)
      (attach-tag 'rectangular (cons x y)))
    (define (make-from-mag-angle-rectangular r a)
      (attach-tag 'rectangular
                  (cons (* r (cos a))
                        (* r (sin a)))))))

(define-library (sicp complex-num-lib-polar)
  (export real-part-polar
          imag-part-polar
          magnitude-polar
          angle-polar
          make-from-real-imag-polar
          make-from-mag-angle-polar)
  (import (scheme base)
          (scheme inexact)
          (sicp type-tags))
  (begin
    (define (real-part-polar z)
      (* (magnitude-polar z)
         (cos (angle-polar z))))
    (define (imag-part-polar z)
      (* (magnitude-polar z)
         (sin (angle-polar z))))
    (define (magnitude-polar z)
      (car z))
    (define (angle-polar z)
      (cdr z))
    (define (make-from-real-imag-polar x y)
      (attach-tag 'polar
                  (cons (sqrt (+ (square x) (square y)))
                        (atan y x))))
    (define (make-from-mag-angle-polar r a)
      (attach-tag 'polar
                  (cons r a)))))

(define-library (sicp complex-num-lib)
  (export add-complex
          sub-complex
          mul-complex
          div-complex
          real-part
          imag-part
          magnitude
          angle
          make-from-real-imag
          make-from-mag-angle)
  (import (scheme base)
          (scheme inexact)
          (sicp type-tags)
          (sicp complex-num-lib-rectangular)
          (sicp complex-num-lib-polar))
  (begin
    (define (rectangular? z)
      (eq? (type-tag z) 'rectangular))
    (define (polar? z)
      (eq? (type-tag z) 'polar))
    (define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
      (make-from-mag-angle (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
      (make-from-mag-angle (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))
    (define (real-part z)
      (cond ((rectangular? z)
             (real-part-rectangular (contents z)))
            ((polar? z)
             (real-part-polar (contents z)))
            (else (error "Unknown type " z))))
    (define (imag-part z)
      (cond ((rectangular? z)
             (imag-part-rectangular (contents z)))
            ((polar? z)
             (imag-part-polar (contents z)))
            (else (error "Unknown type " z))))
    (define (magnitude z)
      (cond ((rectangular? z)
             (magnitude-rectangular (contents z)))
            ((polar? z)
             (magnitude-polar (contents z)))))
    (define (angle z)
      (cond ((rectangular? z)
             (angle-rectangular (contents z)))
            ((polar? z)
             (angle-polar (contents z)))
            (else (error "Unknown type " z))))
    (define (make-from-real-imag x y)
      (make-from-real-imag-rectangular x y))
    (define (make-from-mag-angle r a)
      (make-from-mag-angle-polar r a))))
