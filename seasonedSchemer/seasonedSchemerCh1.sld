(define-library (seasoned-schemer ch1)
  (export letcc
          try
          ch1test)
  (import (scheme base)
          (scheme write))
  (begin
    (define-syntax letcc 
      (syntax-rules () 
        ((letcc var body ...) 
         (call-with-current-continuation 
          (lambda (var)  body ... ))))) 
    (define-syntax try 
      (syntax-rules () 
        ((try var a . b) 
         (letcc success 
                (letcc var (success a)) . b)))) 
    (define ch1test
      (lambda (x)
        x))))
