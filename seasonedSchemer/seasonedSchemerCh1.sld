(include "../littleSchemer/littleSchemerCh1.sld")
(include "../littleSchemer/littleSchemerCh2.sld")
(include "../littleSchemer/littleSchemerCh4.sld")
(define-library (seasoned-schemer ch1)
  (export letcc
          try
          two-in-a-row?)
  (import (scheme base)
          (scheme write)
          (little-schemer ch1)
          (little-schemer ch2)
          (little-schemer ch4))
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
    (define two-in-a-row?
      (lambda (lat)
        (cond ((null? lat) #f)
              (else #f))))))
