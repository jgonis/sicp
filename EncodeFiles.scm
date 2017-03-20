(include "huffman.sld")
(import (scheme file))
(import (scheme write))
(import (scheme process-context))

(define (open-file)
  (define (output-contents contents)
    (define (output-helper output-port)
      (write-string contents output-port))
      (let ((output-file "./testinputs/test2.txt"))
        (call-with-output-file output-file output-helper)))
  (define (input-func input-port)
    (define (input-helper input-port contents length)
      (let ((current-char (read-char input-port)))
        (cond ((eof-object? current-char) (output-contents contents))
              (else (input-helper input-port
                            (string-append contents
                                           (string current-char))
                            (+ length 1))))))
    (input-helper input-port (make-string 0) 0))
  (let ((input-file "./testinputs/test.txt"))
    (cond ((file-exists? input-file)
           (call-with-input-file input-file input-func)))))

(define (parse-file)
  (define (input-func input-port)
    (define (input-helper input-port contents)
      (let ((current-char (read-char input-port)))
        (cond ((eof-object? current-char) contents)
              (else (input-helper
                     input-port
                     (string-append contents
                                    (string current-char)))))))
    (input-helper input-port (make-string 0)))
  (let ((input-file "./testinputs/testfile.txt"))
    (cond ((file-exists? input-file)
           (call-with-input-file input-file input-func)))))
