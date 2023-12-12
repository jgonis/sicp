(define-library (concabs ch4)
  (export verify
	  mod+
	  mod-
	  alt-mod-
	  mod-expt
	  gold-num
	  signature
	  modulus
	  signing-exponent
	  c-curve
	  sierpinskis-gasket)
  (import (scheme base)
          (scheme write)
	  (scheme case-lambda)
	  (scheme time)
	  (scheme inexact)
	  (srfi 1)
	  (srfi 27)
	  (concabs helpers)
	  (concabs fungraph-svg))
  (begin
    (define verify
      (lambda (signature modulus)
	(mod-expt signature 3 modulus)))

    (define mod+
      (lambda (x y modulus)
	(remainder (+ x y) modulus)))

    (define mod-
      (lambda (x y modulus)
	(mod+ x (- modulus y) modulus)))

    (define alt-mod-
      (lambda (x y modulus)
	(let ((inverse-y (remainder (- modulus y) modulus)))
	  (remainder (+ x inverse-y) modulus))))

    (define mod-expt
      (lambda (base exponent modulus)
	(define mod*
	  (lambda (x y)
	    (remainder (* x y) modulus)))
	(cond ((= exponent 0) 1)
	      ((even? exponent) (let ((temp (mod-expt base (/ exponent 2) modulus)))
				  (mod* temp temp)))
	      (else (mod* (mod-expt base (- exponent 1) modulus) base)))))
    
    (define gold-num 5972304273877744135569338397692020533504)
    (define signature 143676221783307728140118556730532825709962359695147398872633033728948225540940112091576952965868445265161373616153020167902900930324840824269164789456142215776895016041636987254848119449940440885630)
    (define modulus 671629488048603400615365258174985654900765971941961654084193604750896012182890124354255484422321487634816640987992317596893099956961956383454333339584850276505584537663630293912940840460009374858969)
    (define signing-exponent 447752992032402267076910172116657103267177314627974436056129069833930674788593416236170322948214322483305175278012793102392215895931470577163544613600143471679799876664686423606429437389098641670667)

    (define c-curve
      (lambda (x0 y0 x1 y1 level)
	(cond ((= level 0) (line x0 y0 x1 y1))
	      (else (let* ((xmid (/ (+ x0 x1) 2))
			   (ymid (/ (+ y0 y1) 2))
			   (dx (- x1 x0))
			   (dy (- y1 y0))
			   (xa (- xmid (/ dy 2)))
			   (ya (+ ymid (/ dx 2))))
		      (overlay (c-curve x0 y0 xa ya (- level 1))
			       (c-curve xa ya x1 y1 (- level 1))))))))
    (define sierpinskis-gasket
      (lambda (bottom-left-x bottom-left-y bottom-right-x bottom-right-y top-x top-y level . wh)
	(cond ((= level 0) (triangle bottom-left-x
				     bottom-left-y
				     bottom-right-x
				     bottom-right-y
				     top-x
				     top-y))
	      (else (let ((bottom-mid-x (+ bottom-left-x (/ (- bottom-right-x bottom-right-y) 2)))
			  (bottom-mid-y (+ bottom-left-y (/ (- bottom-right-y bottom-right-x) 2)))
			  (left-mid-x (+ bottom-left-x (/ (- top-x bottom-left-x) 2)))
			  (left-mid-y (+ bottom-left-y (/ (- top-y bottom-left-y) 2)))
			  (right-mid-x (+ top-x (/ (- bottom-right-x top-x) 2)))
			  (right-mid-y (+ bottom-right-y (/ (- top-y bottom-right-y) 2))))
		      (overlay (sierpinksis-gasket bottom-left-x
						   bottom-left-y
						   bottom-mid-x
						   bottom-mid-y
						   left-mid-x
						   left-mid-y
						   (- level 1))
			       (sierpinksis-gasket left-mid-x
						   left-mid-y
						   bottom-right-x
						   bottom-right-y
						   right-mid-x
						   right-mid-y
						   (- level 1))
			       (sierpinksis-gasket left-mid-x
						   left-mid-y
						   right-mid-x
						   right-mid-y
						   top-x
						   top-y
						   (- level 1))))))))
    ))
