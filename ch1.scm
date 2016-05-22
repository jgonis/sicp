(define-library (sicp ch1)
  (export square sum-of-squares)
  (import (except (scheme base) square))
  (begin
    (define square
      (lambda (n)
        (* n n)))
    (define sum-of-squares
      (lambda (x y)
        (+ (square x) (square y))))))

(import (srfi :78))
(import (rename (prefix (sicp ch1) sicp-)))
(check-set-mode! 'summary)
(check (sicp-square 5) => 25)
(check (sicp-sum-of-squares 3 4) => 25)
(check-report)
(check-reset!)
