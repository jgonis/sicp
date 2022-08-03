(define-library (seasoned-schemer ch16)
  (export sweet-toothL
          sweet-toothR
          last
          ingredients
          deep
          deepR
          deepM
          find
          alt-find
          alt-deep
          counter
          set-counter!
          supercounter
          rember1*-consCount
          alt-rember1*-consCount)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch4)
          (little-schemer ch5) 
          (seasoned-schemer ch11))
  (begin
    (define last 'angelfood)

    (define ingredients '())

    (define sweet-toothL
      (lambda (food)
        (set! last food)
        (cons food
              (cons 'cake '()))))

    (define sweet-toothR
      (lambda (food)
        (set! ingredients (cons food ingredients))
        (cons food
              (cons 'cake '()))))

    (define deep
      (lambda (depth)
        (cond ((= depth 0) 'pizza)
              (else (cons (deep (- depth 1)) '())))))
    
    (define deepR
      (let ((inputs '()))
        (lambda (depth)
          (set! inputs (cons depth inputs))
          (cond ((= depth 0) 'pizza)
                (else (let ((result (cons (deepR (- depth 1)) '())))
                        (set! inputs (cons (cons depth result) inputs))
                        result))))))

    (define find
      (lambda (n alist)
        (cond ((null? alist) #f)
              ((= n (car (car alist))) (cdr (car alist)))
              (else (find n (cdr alist))))))

    (define alt-find
      (lambda (n ns rs)
        (letrec ((helper (lambda (ns rs)
                           (cond ((null? ns) #f)
                                 ((eq? n (car ns)) (car rs))
                                 (else (helper (cdr ns) (cdr rs)))))))
          (helper ns rs))))

    (define deepM
      (let ((inputs (cons (cons 0 'pizza) '())))
        (lambda (depth conz)
          (let ((in-inputs (find depth inputs)))
            (cond (in-inputs in-inputs)
                  (else (let ((result (conz (deepM (- depth 1) conz) '())))
                          (set! inputs (conz (conz depth result) inputs))
                          result)))))))

    (define counter 0)
    (define set-counter! 0)

    (define consCounter
      (let ((n 0))
        (set! counter
              (lambda ()
                n))
        (set! set-counter!
              (lambda ()
                (set! n 0)))
        (lambda (x y)
          (set! n (+ n 1))
          (cons x y))))

    (define alt-deep
      (lambda (m conz)
        (cond ((= 0 m) 'pizza)
              (else (conz (alt-deep (- m 1) conz)
                          '())))))
    
    (define supercounter
      (lambda (func)
        (let* ((counter 0)
               (alt-cons (lambda (x y)
                           (set! counter (+ counter 1))
                           (cons x y))))
          (letrec ((s (lambda (n)
                        (cond ((= n 0) (func n alt-cons))
                              (else (let ()
                                      (func n alt-cons)
                                      (s (- n 1))))))))
            (s 1000))
          counter)))

    (define alt-rember1*-consCount
      (lambda (a l)
        (letrec ((r (lambda (l)
                      (cond ((null? l) '())
                            ((atom? (car l)) (cond ((eq? (car l) a) (cdr l))
                                                   (else (consCounter (car l)
                                                                      (r (cdr l))))))
                            (else (let ((r-car-l (r (car l))))
                                    (cond ((eqlist? r-car-l
                                                    (car l))
                                           (consCounter (car l) (r (cdr l))))
                                          (else (consCounter r-car-l
                                                             (cdr l))))))))))
          (r l))))


    (define rember1*-consCount
      (lambda (a l)
        (letrec ((rm (lambda (a l oh)
                       (cond ((null? l) (oh 'no))
                             ((atom? (car l)) (if (eq? (car l) a)
                                                  (cdr l)
                                                  (consCounter (car l)
                                                               (rm a (cdr l) oh))))
                             (else (let ((ret-val (letcc oh (rm a (car l) oh))))
                                     (cond ((atom? ret-val) (consCounter (car l)
                                                                         (rm a (cdr l) oh)))
                                           (else (consCounter ret-val (cdr l))))))))))
          (let ((new-l (letcc oh (rm a l oh))))
            (cond ((atom? new-l) l)
                  (else new-l))))))))
