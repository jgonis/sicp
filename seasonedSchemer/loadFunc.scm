;; A centralized place to create a function that will load all of the files in the
;; system that comprise my SICP work so I can work in an iterative fashion
(define (load-func)
  (load "/home/pi/Code/sicp/seasonedSchemer/seasonedSchemerCh11.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/seasonedSchemerCh12.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/seasonedSchemerCh13.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/seasonedSchemerCh14.sld")
  ;; Test Files
  (load "/home/pi/Code/sicp/seasonedSchemer/tests.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/testsCh11.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/testsCh12.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/testsCh13.sld")
  (load "/home/pi/Code/sicp/seasonedSchemer/testsCh14.sld"))
