(include "ch2.scm")
(define-library (sicp ex31)
  (export ex31)
  (import (scheme base))
  (begin
    (define (ex31 n)
      (lambda (x)
        (set! n (+ n x))
        n))))

(define-library (sicp ex32)
  (export ex32)
  (import (scheme base))
  (begin
    (define (ex32 func)
      (let ((call-count 0))
        (lambda (input)
          (cond ((eq? input 'how-many-calls?) call-count)
                (else (set! call-count (+ call-count 1))
                      (func input))))))))

(define-library (sicp ex33)
  (export make-account)
  (import (scheme base))
  (begin
    (define (make-account balance account-password)
      (define (withdraw amount)
        (cond ((>= balance amount) (set! balance (- balance amount))
               balance)
              (else "Insufficient funds")))
      (define (deposit amount)
        (set! balance (+ balance amount))
        balance)
      (define (incorrect-password . args)
        "Incorrect password")
      (define (dispatch password m)
        (cond ((eq? password account-password)
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else (error "Unknown request" m))))
              (else incorrect-password)))
      dispatch)))

(define-library (sicp ex34)
  (export make-account)
  (import (scheme base))
  (begin
    (define (make-account balance account-password)
      (let ((login-tries 0))
        (define (withdraw amount)
          (cond ((>= balance amount) (set! balance (- balance amount))
                 balance)
                (else "Insufficient funds!")))
        (define (deposit amount)
          (set! balance (+ balance amount))
          balance)
        (define (get-balance)
          balance)
        (define (call-the-cops . args)
          "Calling the cops!")
        (define (incorrect-password . args)
          "Incorrect password")
        (define (dispatch password m)
          (cond ((eq? password account-password)
                 (set! login-tries 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       ((eq? m 'balance) get-balance)
                       (else (error "Unknown message" m))))
                (else (cond ((>= login-tries 7) call-the-cops)
                            (else (set! login-tries (+ login-tries 1))
                                  incorrect-password)))))
        dispatch))))

(define-library (sicp monte-carlo)
  (export monte-carlo)
  (import (scheme base))
  (begin
    (define (monte-carlo trials experiment)
      (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)))
              (else (iter (- trials-remaining 1)
                          trials-passed))))
      (iter trials 0))))

(define-library (sicp monte-carlo-pi)
  (export estimate-pi)
  (import (scheme base)
          (srfi 27)
          (scheme inexact)
          (sicp monte-carlo))
  (begin
    (define (estimate-pi trials)
      (sqrt (/ 6 (monte-carlo trials cesaro-test))))
    (define (cesaro-test)
      (= (gcd (random-integer 2147483647)
              (random-integer 2147483647))
         1))))

(define-library (sicp ex35)
  (export estimate-integral
          area-predicate)
  (import (scheme base)
          (srfi 27)
          (sicp monte-carlo))
  (begin
    (define (area-predicate x1 x2 y1 y2 circle-rad circle-x circle-y)
      (lambda ()
        (let* ((x-range (- x2 x1))
               (y-range (- y2 y1))
               (x-point (+ x1 (* (random-real) x-range)))
               (y-point (+ y1 (* (random-real) y-range)))
               (in-circle (<= (+ (square (- x-point circle-x))
                                 (square (- y-point circle-y)))
                              (square circle-rad))))
                   in-circle)))
    (define (estimate-integral pred x1 x2 y1 y2 trials)
      (let ((area (* (- x2 x1) (- y2 y1))))
        (* area (monte-carlo trials pred))))
    (define (random-in-range low high)
      (let ((range (- high low)))
        (+ low (random-integer range))))))


(define-library (sicp ex38)
  (export ex38)
  (import (scheme base)
          (sicp ex34))
  (begin
    (define (ex38 account old-password new-password)
      (define (dispatch password m)
        (cond ((equal? password new-password)
               (account old-password m))
              (else (error "Unknown password"))))
      dispatch)))


;; Ex 3.12 - (define x (list 'a 'b))
;;           (define y (list 'c 'd))
;;           (define z (append x y))
;;           (cdr x) -> (b)
;;           (define w (append! x y))
;;           (cdr x) -> (b c d)

;; Ex 3.13 - The make-cycle function makes a circularly linked list
;; Calling last-pair on the result of that function will cause an
;; infinite loop.

(define-library (sicp ex314)
  (export mystery)
  (import (scheme base))
  (begin
    (define (mystery x)
      (define (loop x y)
        (cond ((null? x) y)
              (else (let ((temp (cdr x)))
                      (set-cdr! x y)
                      (loop temp x)))))
      (loop x '()))))
;; Calling (mystery '(a b c d)) will result in (d c b a) as the
;; mystery function reverses lists.
(define-library (sicp ex316)
  (export count-pairs)
  (import (scheme base))
  (begin
    (define (count-pairs x)
      (cond ((not (pair? x)) 0)
            (else (+ (count-pairs (car x))
                     (count-pairs (cdr x))
                     1))))))

;; Ex 3.16
;;3 pairs, returns 3 count
;;(define p3 (cons 3 '()))
;;(define p2 (cons 2 p3))
;;(define p1 (cons 1 p2))
;; (count-pairs p1)  -> 3

;;3 pairs, returns 4 count
;;(set-car! p1 p2)
;;(set-cdr! p1 p3)
;;(set-car! p2 p3)
;; (count-pairs p1) -> 4

;;3 pairs, returns 7 count
;;(set-car! p1 p2)
;;(set-cdr! p1 p2)
;;(set-car! p2 p3)
;;(set-cdr! p2 p3)
;;(set-car! p3 1)
;;(count-pairs p1) -> 7

;;3 pairs, never returns at all
;;(set-car! p1 p2)
;;(set-car! p2 p1)
;; (count-pairs p1) -> infinite loop

(define-library (sicp ex317)
  (export correct-count-pairs)
  (import (scheme base))
  (begin
        (define (correct-count-pairs x)
          (let ((visited '()))
            (define (find-in-list item lyst)
              (cond ((null? lyst) #f)
                    ((eq? item (car lyst)) #t)
                    (else (find-in-list item (cdr lyst)))))
            (define (helper x)
              (cond ((not (pair? x)) 0)
                    ((find-in-list x visited) (+ (helper (car x))
                                                 (helper (cdr x))))
                    (else (set! visited (cons x visited))
                          (+ 1
                             (helper (car x))
                             (helper (cdr x))))))
            (helper x)))))

(define-library (sicp ex318)
  (export contains-cycle?)
  (import (scheme base))
  (begin
    (define (contains-cycle? lyst)
      (define (find-in-list item lyst)
        (cond ((null? lyst) #f)
              ((eq? item (car lyst)) #t)
              (else (find-in-list item (cdr lyst)))))
      (define (helper x visited)
        (cond ((not (pair? x)) #f)
              ((find-in-list x visited) #t)
              (else (or (helper (car x) (cons x visited))
                        (helper (cdr x) (cons x visited))))))
      (helper lyst '()))))

(define-library (sicp ex319)
  (export ex319)
  (import (scheme base))
  (begin
    (define (ex319 lyst)
      #f)))

;;Tortoise and Hare algorith.
;;Tortoise goes 1 cdr forward, hare goes 2 cdrs forward
;;at each step check to see if node tortoise is at is eq?
;;to node hare is at.  If so you have a cycle.
;;Also need to deal with a node pointing to a
