(define-library (seasoned-schemer ch18)
  (export lots
          add-at-end
          add-at-end-too
          kons
          kar
          kdr
          same?
          last-cdr
          finite-length)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define lots
      (lambda (x)
        (cond ((= 0 x) '())
              (else (cons 'egg
                          (lots (- x 1)))))))

    (define add-at-end
      (lambda (a l)
        (cond ((null? l) (cons a '()))
              (else (cons (car l)
                          (add-at-end a (cdr l)))))))

    (define add-at-end-too
      (lambda (a l)
        (letrec ((helper (lambda (ls)
                           (cond ((null? (cdr ls)) (set-cdr! ls (cons a '())))
                                 (else (helper (cdr ls)))))))
          (cond ((null? l) (cons a '()))
                (else (begin
                        (helper l)
                        l))))))

    (define kons
      (lambda (kar kdr)
        (lambda (selector)
          (selector kar kdr))))
    
    (define kar
      (lambda (kons-obj)
        (let ((kar-proc (lambda (first rest) first)))
          (kons-obj kar-proc))))

    (define kdr
      (lambda (kons-obj)
        (let ((kdr-proc (lambda (first rest) rest)))
          (kons-obj kdr-proc))))

    (define same?
      (lambda (c1 c2)
        (cond ((or (null? c1) (null? c2)) #f)
              (else
               (let ((t1 (cdr c1))
                     (t2 (cdr c2)))
                 (set-cdr! c1 1)
                 (set-cdr! c2 2)
                 (let ((v (= (cdr c1) (cdr c2))))
                   (set-cdr! c1 t1)
                   (set-cdr! c2 t2)
                   v))))))

    (define last-cdr
      (lambda (ls)
        (cond ((null? (cdr ls)) ls)
              (else (last-cdr (cdr ls))))))

    (define finite-length
      (lambda (p)
        (letcc infinite
               (letrec ((C (lambda (p q)
                             (cond ((same? p q) (infinite #f))
                                   ((null? q) 0)
                                   ((null? (cdr q)) 1)
                                   (else (+ (C (sl p) (qk q))
                                            2)))))
                        (qk (lambda (x) (cdr (cdr x))))
                        (sl (lambda (x) (cdr x))))
                 (cond ((null? p) 0)
                       (else (+ 1 (C p (cdr p)))))))))))
