;;java -jar kawa.jar -s -r7rs
(define-library (sicp ch1 ch1)
  (export test-func)
  (import (scheme base))
  (begin
    (define test-func
      (lambda (x)
        (cond ((= x 0) 1)
              (else (+ 1 (* x (test-func (- x 1))))))))))
