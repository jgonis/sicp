;; A centralized place to create a function that will load all of the files in the
;; system that comprise my SICP work so I can work in an iterative fashion
(define (load-func directory-path)
  (let* ((function-file-strings (list "littleSchemer/littleSchemerCh1.sld"
                                      "littleSchemer/littleSchemerCh2.sld"
                                      "littleSchemer/littleSchemerCh3.sld"
                                      "littleSchemer/littleSchemerCh4.sld"
                                      "littleSchemer/littleSchemerCh5.sld"
                                      "littleSchemer/littleSchemerCh6.sld"
                                      "littleSchemer/littleSchemerCh7.sld"
                                      "littleSchemer/littleSchemerCh8.sld"
                                      "littleSchemer/littleSchemerCh9.sld"
                                      "littleSchemer/littleSchemerCh10.sld"))
         (test-file-strings (list "littleSchemer/testsCh1.sld"
                                  "littleSchemer/testsCh2.sld"
                                  "littleSchemer/testsCh3.sld"
                                  "littleSchemer/testsCh4.sld"
                                  "littleSchemer/testsCh5.sld"
                                  "littleSchemer/testsCh6.sld"                             
                                  "littleSchemer/testsCh7.sld"
                                  "littleSchemer/testsCh8.sld"
                                  "littleSchemer/testsCh9.sld"
                                  "littleSchemer/testsCh10.sld"
                                  "littleSchemer/tests.sld"))
         (full-paths (map (lambda (file-string) (string-append directory-path
                                                          file-string))
                          (append function-file-strings
                                  test-file-strings))))
    (for-each (lambda (full-path) (load full-path))
              full-paths)))

