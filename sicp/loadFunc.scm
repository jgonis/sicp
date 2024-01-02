;; a centralized place to create a function that will load all of the files in the
;; system that comprise my SICP work so I can work in an iterative fashion
(define (load-func directory-path)
  (let* ((function-file-strings (list
                                 "sicp/ch1/ch1.scm"               
                                 "sicp/ch2/ch2.scm"               
                                 "sicp/ch2/rationalNumbers.scm"   
                                 "sicp/ch2/point.scm"             
                                 "sicp/ch2/segment.scm"           
                                 "sicp/ch2/rectangle.scm"         
                                 "sicp/ch2/rectangleFunctions.scm"
                                 "sicp/ch2/altRectangle.scm"      
                                 "sicp/ch2/intervals.scm"         
                                 "sicp/ch2/altIntervals.scm"   
                                 "sicp/ch2/intervalFunctions.scm"
                                 "sicp/ch2/tree-library.scm"
                                 "sicp/libs/fp-compare.scm"
                                 "sicp/libs/helpers.scm"))
         (test-file-strings (list
                             "sicp/tests/ch1/ch1Tests.scm"           
                             "sicp/tests/ch2/ch2Tests.scm"           
                             "sicp/tests/ch2/pointTests.scm"         
                             "sicp/tests/ch2/segmentTests.scm"       
                             "sicp/tests/ch2/rectangleTests.scm"     
                             "sicp/tests/ch2/intervalTests.scm"      
                             "sicp/tests/ch2/rationalNumberTests.scm"
                             "sicp/tests/ch2/treeTests.scm"))
         (full-paths (map (lambda (file-string) (string-append directory-path
                                                          file-string))
                          (append function-file-strings
                                  test-file-strings))))
    (for-each (lambda (full-path) (load full-path))
              full-paths)))
