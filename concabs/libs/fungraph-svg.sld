(define-library (concabs fungraph-svg)
  (export make-image
	  image?
	  image-width
	  image-height
	  save-image-as-svg
	  line
	  filled-triangle
          filled-rectangle
	  quarter-turn-right
	  half-turn
	  quarter-turn-left
	  side-by-side
	  stack
	  overlay
	  resize-image
          mirror-image
          invert
          pinwheel
          stack-of-copies
          stack-of-alternating-copies)
  (import (scheme base)
	  (scheme write)
	  (scheme file))
  (begin
    (define default-image-size 100)

    (define-record-type <image>
      (make-image image-proc width height)
      image?
      (width image-width)
      (height image-height)
      (image-proc image-proc))

    (define save-image-as-svg
      (lambda (image filename)
	(if (not (image? image))
	    (error "argument to save-image-as-svg not an image" image))
	(with-output-to-file filename
	  (lambda ()
	    (display (string-append "<svg version=\"1.1\"\n"
				    "width=\"" (number->string (image-width image)) "\"\n"
				    "height=\"" (number->string (image-height image)) "\"\n"
				    "xmlns=\"http://www.w3.org/2000/svg\">\n"
                                    "<defs>\n"
                                    "<filter id=\"MyInvert\">\n"
                                    "<feComponentTransfer>\n"
                                    "<feFuncR type=\"table\" tableValues=\"1 0\" />\n"
                                    "<feFuncG type=\"table\" tableValues=\"1 0\" />\n"
                                    "<feFuncB type=\"table\" tableValues=\"1 0\" />\n"
                                    "</feComponentTransfer>\n"
                                    "</filter>\n"
                                    "</defs>\n"
				    "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"))
	    ((image-proc image)
	     (lambda (x y) (* (+ x 1) (/ (image-width image) 2.0)))
	     (lambda (x y) (* (+ (* y -1) 1) (/ (image-height image) 2.0))))
	    (display "</svg>")
	    (newline)))))

    (define (line x0 y0 x1 y1 . wh)
      (if (not (real? x0))
	  (error "x0 argument to line not a real" x0))
      (if (not (real? x1))
	  (error "x1 argument to line not a real" x1))
      (if (not (real? y0))
	  (error "y0 argument to line not a real" y0))
      (if (not (real? y1))
	  (error "y1 argument to line not a real" y1))
      (let ((width default-image-size)
            (height default-image-size))
	(if (not (null? wh))
            (begin (set! width (car wh))
		   (if (not (null? (cdr wh)))
                       (begin (set! height (cadr wh))
                              (if (not (null? (cddr wh)))
				  (error "too many arguments to line")))
                       (set! height width))))
	(if (not (and (integer? height)
                      (integer? width)
                      (exact? height)
                      (exact? width)
                      (> height 0)
                      (> width 0)))
            (error "illegal size specification in line" wh))
	(make-image (lambda (x-transformer y-transformer)
		      (display (string-append "<path d=\"M "
					      (number->string (inexact (x-transformer x0 y0))) " "
					      (number->string (inexact (y-transformer x0 y0))) " "
					      "L "
					      (number->string (inexact (x-transformer x1 y1))) " "
					      (number->string (inexact (y-transformer x1 y1))) "\""
					      "/>"))
                      (newline))
                    width height)))

    (define (filled-triangle x0 y0 x1 y1 x2 y2 . wh)
      (if (not (real? x0))
	  (error "x0 argument to filled-triangle not a real" x0))
      (if (not (<= -1 x0 1))
	  (error "x0 argument to filled-triangle not in -1 to 1 range" x0))
      (if (not (real? x1))
	  (error "x1 argument to filled-triangle not a real" x1))
      (if (not (<= -1 x1 1))
	  (error "x1 argument to filled-triangle not in -1 to 1 range" x1))
      (if (not (real? x2))
	  (error "x2 argument to filled-triangle not a real" x2))
      (if (not (<= -1 x2 1))
	  (error "x2 argument to filled-triangle not in -1 to 1 range" x2))
      (if (not (real? y0))
	  (error "y0 argument to filled-triangle not a real" y0))
      (if (not (<= -1 y0 1))
	  (error "y0 argument to filled-triangle not in -1 to 1 range" y0))
      (if (not (real? y1))
	  (error "y1 argument to filled-triangle not a real" y1))
      (if (not (<= -1 y1 1))
	  (error "y1 argument to filled-triangle not in -1 to 1 range" y1))
      (if (not (real? y2))
	  (error "y2 argument to filled-triangle not a real" y2))
      (if (not (<= -1 y2 1))
	  (error "y2 argument to filled-triangle not in -1 to 1 range" y2))
      (let ((width default-image-size)
            (height default-image-size))
	(if (not (null? wh))
            (begin (set! width (car wh))
		   (if (not (null? (cdr wh)))
                       (begin (set! height (cadr wh))
                              (if (not (null? (cddr wh)))
				  (error "too many argument to filled-triangle")))
                       (set! height width))))
	(if (not (and (integer? height)
                      (integer? width)
                      (exact? height)
                      (exact? width)
                      (> height 0)
                      (> width 0)))
            (error "illegal size specification in filled-triangle" wh))
	(make-image (lambda (x-transformer y-transformer)
		      (display (string-append "<path d=\"M "
					      (number->string (inexact (x-transformer x0 y0))) " "
					      (number->string (inexact (y-transformer x0 y0))) " "
					      "L "
					      (number->string (inexact (x-transformer x1 y1))) " "
					      (number->string (inexact (y-transformer x1 y1))) " "
					      "L "
					      (number->string (inexact (x-transformer x2 y2))) " "
					      (number->string (inexact (y-transformer x2 y2))) " "
					      "Z\" fill=\"black\""
					      "/>"))
                      (newline))
                    width
		    height)))

    (define filled-rectangle
      (lambda (x0 y0 x1 y1)
        (overlay (filled-triangle x0 y0 x0 y1 x1 y1)
                 (filled-triangle x0 y0 x1 y0 x1 y1))))

    (define (mirror-image image)
      (if (not (image? image))
	  (error "argument to mirror-image not an image" image))
      (make-image (lambda (xt yt)
                    ((image-proc image) (lambda (x y) (xt (- x) y))
                     (lambda (x y) (yt (- x) y))))
		  (image-width image)
		  (image-height image)))

    (define (invert image)
      (if (not (image? image))
	  (error "argument to invert not an image" image))
      (make-image (lambda (xt yt)
                    (let* ((left (inexact (xt -1 -1)))
			   (bottom  (inexact (yt -1 -1)))
			   (right (inexact (xt 1 1)))
			   (top (inexact (yt 1 1)))
                           (width (- right left))
                           (height (- bottom top)))
                      (display "<g filter=\"url(#MyInvert)\">")
                      (newline)
                      (display (string-append "<rect x=\""
                                              (number->string left)
                                              "\" y=\""
                                              (number->string top)
                                              "\" width=\""
                                              (number->string width)
                                              "\" height=\""
                                              (number->string height)
                                              "\" fill=\"white\"/>"))
                      (newline)
                      ((image-proc image) xt yt)
                      (display "</g>")
                      (newline)))
		  (image-width image)
		  (image-height image)))

    (define (overlay image . images)
      (if (not (image? image))
	  (error "argument to overlay not an image" image))
      (let ((w (image-width image))
            (h (image-height image)))
	(for-each
	 (lambda (i)
	   (if (not (image? i))
               (error "argument to overlay not an image" i))
	   (if (not (and (= (image-width i) w)
			 (= (image-height i) h)))
               (error "Only images of equal size can be overlayed"
                      (cons image images))))
	 images)
	(make-image
	 (lambda (xt yt)
	   (for-each
            (lambda (image) ((image-proc image) xt yt))
            (cons image images)))
	 w h)))

    (define (stack top . rest)
      (define (stack2 top bottom)
	(if (not (image? top))
            (error "argument to stack not an image" top))
	(if (not (image? bottom))
            (error "argument to stack not an image" bottom))
	(if (not (= (image-width top) (image-width bottom)))
            (error "Attempt to stack images of different widths" (list top bottom))
            (let ((th (image-height top))
		  (bh (image-height bottom)))
              (let* ((h (+ th bh))
                     (inexact-h (inexact h)))
		(let ((tscale (/ th inexact-h))
                      (bscale (/ bh inexact-h)))
		  (make-image
		   (lambda (xt yt)
                     ((image-proc top) (lambda (x y)
					 (xt x (+ (* tscale y) bscale)))
                      (lambda (x y)
                        (yt x (+ (* tscale y) bscale))))
                     ((image-proc bottom) (lambda (x y)
                                            (xt x (- (* bscale y) tscale)))
                      (lambda (x y)
                        (yt x (- (* bscale y) tscale)))))
		   (image-width top)
		   h))))))
      (let loop ((image top)
		 (images rest))
	(if (null? images)
            image
            (loop (stack2 image (car images)) (cdr images)))))

    (define (quarter-turn-right image)
      (if (not (image? image))
	  (error "argument to quarter-turn-right not an image" image))
      (make-image (lambda (xt yt)
                    ((image-proc image) (lambda (x y) (xt y (- x)))
                     (lambda (x y) (yt y (- x)))))
		  (image-height image)
		  (image-width image)))
    
    (define (half-turn image)
      (if (not (image? image))
	  (error "argument to half-turn not an image" image))
      (quarter-turn-right (quarter-turn-right image)))

    (define (quarter-turn-left image)
      (if (not (image? image))
	  (error "argument to quarter-turn-left not an image" image))
      (quarter-turn-right (quarter-turn-right (quarter-turn-right image))))
    
    (define (side-by-side left-image right-image)
      (if (not (image? left-image))
	  (error "left-image argument to side-by-side not an image" left-image))
      (if (not (image? right-image))
	  (error "right-image argument to side-by-side not an image" right-image))
      (quarter-turn-right (stack (quarter-turn-left right-image) (quarter-turn-left left-image))))

    (define (mirror-image image)
      (if (not (image? image))
	  (error "argument to mirror-image not an image" image))
      (make-image (lambda (xt yt)
                    ((image-proc image) (lambda (x y) (xt (- x) y))
                     (lambda (x y) (yt (- x) y))))
		  (image-width image)
		  (image-height image)))

    (define (resize-image image . wh)
      (if (not (image? image))
	  (error "argument to resize-image not an image" image))
      (let ((width default-image-size)
            (height default-image-size))
	(if (not (null? wh))
            (begin (set! width (car wh))
		   (if (not (null? (cdr wh)))
                       (begin (set! height (cadr wh))
                              (if (not (null? (cddr wh)))
				  (error "too many argument to resize-image")))
                       (set! height width))))
	(if (not (and (integer? height)
                      (integer? width)
                      (exact? height)
                      (exact? width)
                      (> height 0)
                      (> width 0)))
            (error "illegal size specification in resize-image" wh))
	(make-image (image-proc image) width height)))

    (define pinwheel
      (lambda (image)
        (side-by-side (stack (quarter-turn-right image) image)
                      (stack (half-turn image) (quarter-turn-left image)))))

    (define stack-of-copies
      (lambda (n image)
        (let loop ((i 1)
                   (stacked-image image))
          (cond ((< i n) (loop (+ i 1) (stack image stacked-image)))
                (else stacked-image)))))
    (define stack-of-alternating-copies
      (lambda (n image)
        (let loop ((i 1)
                   (stacked-image image))
          (cond ((and (< i n) (even? i))
                 (loop (+ i 1)
                       (stack image stacked-image)))
                ((and (< i n) (odd? i))
                 (loop (+ i 1)
                       (stack (invert image) stacked-image)))
                (else stacked-image)))))))

