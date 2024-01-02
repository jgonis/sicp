;; A centralized place to create a function that will load all of the files in the
;; system that comprise my SICP work so I can work in an iterative fashion
(define (load-func directory-path)
  (let* ((function-file-strings (list "seasonedSchemer/seasonedSchemerCh11.sld"
                                      "seasonedSchemer/seasonedSchemerCh12.sld"
                                      "seasonedSchemer/seasonedSchemerCh13.sld"
                                      "seasonedSchemer/seasonedSchemerCh14.sld"
                                      "seasonedSchemer/seasonedSchemerCh16.sld"
                                      "seasonedSchemer/seasonedSchemerCh18.sld"
                                      "seasonedSchemer/seasonedSchemerCh19.sld"
                                      "seasonedSchemer/seasonedSchemerCh20.sld"))
         (test-file-strings (list "seasonedSchemer/testsCh11.sld"
                                  "seasonedSchemer/testsCh12.sld"
                                  "seasonedSchemer/testsCh13.sld"
                                  "seasonedSchemer/testsCh14.sld"
                                  "seasonedSchemer/testsCh16.sld"
                                  "seasonedSchemer/testsCh18.sld"                             
                                  "seasonedSchemer/testsCh19.sld"
                                  "seasonedSchemer/testsCh20.sld"
                                  "seasonedSchemer/tests.sld"))
         (full-paths (map (lambda (file-string) (string-append directory-path
                                                          file-string))
                          (append function-file-strings
                                  test-file-strings))))
    (for-each (lambda (full-path) (load full-path))
              full-paths)))
