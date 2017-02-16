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
