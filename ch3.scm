(include "ch2.scm")
(define-library (sicp ex31)
  (export ex31)
  (import (scheme base))
  (begin
    (define (ex31 n)
      (lambda (x)
        (set! n (+ n x))
        n))))
