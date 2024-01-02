(define (load-func directory-path)
  (let* ((file-strings (list "concabs/libs/fungraph-svg.sld" 
                             "concabs/libs/quilting.sld"
                             "concabs/libs/digits.sld"       
                             "concabs/libs/helpers.sld"
                             "concabs/ch2.scm"      
                             "concabs/ch3.scm"      
                             "concabs/ch4.scm"      
                             "concabs/ch2Tests.scm"))
         (full-paths (map (lambda (file-string) (string-append directory-path
                                                          file-string))
                          file-strings)))
    (for-each (lambda (full-path) (load full-path))
              full-paths)))


(define (load-old-func)
  (let* ((file-strings (list "concabs/libs/old-libs/fungraph.scm"
                             "concabs/libs/old-libs/quilting.scm"))
         (full-paths (map (lambda (file-string) (string-append directory-path
                                                          file-string))
                          file-strings)))
    (for-each (lambda (full-path) (load full-path))
              full-paths)))
