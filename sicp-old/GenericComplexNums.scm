(include "typeTags.sld")

(define-library (sicp generic-lib)
  (export get-func
          put-func
          get-func-list
          reset-func-list
          apply-generic)
  (import (scheme base)
          (sicp type-tags))
  (begin
    (define (get-func op type func-list)
      (define (helper op type func-list)
        (cond ((null? func-list) #f)
              ((and (eq? op (car (car (car func-list))))
                    (equal? type (cdr (car (car func-list)))))
               (cdr (car func-list)))
              (else (helper op type (cdr func-list)))))
      (helper op type func-list))
    (define (put-func op type func func-list)
      (set! func-list (cons (cons (cons op type) func) func-list)))
    (define (reset-func-list func-list)
      (set! func-list '()))
    (define (apply-generic op . args)
      (let* ((type-tags (map type-tag args))
             (proc (get-func op type-tags)))
        (cond (proc (apply proc (map contents args)))
              (else (error "No method for these types"
                           (list op type-tags))))))))

(define-library (sicp complex-generic-ops)
  (export (rename real-part jreal-part)
          (rename angle jangle)
          (rename magnitude jmagnitude)
          (rename imag-part jimag-part))
  (import (except (scheme base) magnitude angle real-part imag-part)
          (sicp generic-lib))
  (begin
    (define (real-part z)
      (apply-generic 'real-part z))
    (define (imag-part z)
      (apply-generic 'imag-part z))
    (define (magnitude z)
      (apply-generic 'magnitude z))
    (define (angle z)
      (apply-generic 'angle z))))

(define-library (sicp complex-num-lib-polarv2)
  (export install-polar-package)
  (import (scheme base)
          (scheme inexact)
          (sicp type-tags)
          (sicp generic-lib))
  (begin
   (define (install-polar-package)
     (define (magnitude z) (car z))
     (define (angle z) (cdr z))
     (define (make-from-mag-angle r a)
       (cons r a))
     (define (real-part z)
       (* (magnitude z) (cos (angle z))))
     (define (imag-part z)
       (* (magnitude z) (sin (angle z))))
     (define (make-from-real-imag x y)
       (cons (sqrt (+ (square x) (square y)))
             (atan y x)))
     (define (tag x) (attach-tag 'polar x))
     (put-func 'real-part '(polar) real-part)
     (put-func 'imag-part '(polar) imag-part)
     (put-func 'magnitude '(polar) magnitude)
     (put-func 'angle '(polar) angle)
     (put-func 'make-from-real-imag
               'polar
               (lambda (x y) (tag (make-from-real-imag x y))))
     (put-func 'make-from-mag-ang
               'polar
               (lambda (r a) (tag (make-from-mag-angle r a))))
     'done)))

(define-library (sicp complex-num-lib-v2)
  (export make-from-real-imag
          make-from-mag-ang
          add-complex
          sub-complex
          mul-complex
          div-complex)
  (import (scheme base)
          (sicp generic-lib)
          (prefix (sicp complex-generic-ops) j-)
          (sicp complex-num-lib-polarv2))
  (begin
    (define (make-from-real-imag x y)
      ((get-func 'make-from-real-imag 'rectangular) x y))
    (define (make-from-mag-ang r a)
      ((get-func 'make-from-mag-ang 'polar) r a))
    (define (add-complex z1 z2)
      (make-from-real-imag (+ (j-real-part z1) (j-real-part z2))
                           (+ (j-imag-part z1) (j-imag-part z2))))
    (define (sub-complex z1 z2)
      (make-from-real-imag (- (j-real-part z1) (j-real-part z2))
                            (- (j-imag-part z1) (j-imag-part z2))))
    (define (mul-complex z1 z2)
      (make-from-mag-ang (* (j-magnitude z1) (j-magnitude z2))
                           (+ (j-angle z1) (j-angle z2))))
    (define (div-complex z1 z2)
      (make-from-mag-ang (/ (j-magnitude z1) (j-magnitude z2))
                         (- (j-angle z1) (j-angle z2))))))


(define (arg-set-test arg)
  (set! arg (cons 1 2))
  'done)
