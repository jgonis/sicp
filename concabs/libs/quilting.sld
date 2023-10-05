(define-library (concabs quilting)
  (export test-bb
	  nova-bb
	  bitw-bb
	  rcross-bb
	  corner-bb
          quilt
          checkerboard)
  (import (scheme base)
	  (scheme cxr)
	  (concabs fungraph-svg))
(begin
  ;; A simple test image, to illustrate transformations.
  (define test-bb
    (filled-triangle 0 1 0 -1 1 -1))

  ;; This one only has two triangles, but makes an interesting pinwheel.
  (define nova-bb
    (overlay (filled-triangle 0 1 0 0 -1/2 0)
             (filled-triangle 0 0 0 1/2 1 0)))

  ;; Basic block for "Blowing in the Wind" quilting pattern from
  ;; "Quick-and-Easy Strip Quilting" by Helen Whitson Rose, Dover
  ;; Publications, New York, 1989, p. 59.
  (define bitw-bb
    (overlay (overlay (filled-triangle -1 1 0 1 -1/2 1/2)
		      (filled-triangle -1 -1 0 -1 -1 0))
             (overlay (filled-triangle 1 1 1 0 0 0)
		      (filled-triangle 0 0 1 0 1/2 -1/2))))
  
  ;; Basic block for "Repeating Crosses" quilting pattern from
  ;; "Quick-and-Easy Strip Quilting" by Helen Whitson Rose, Dover
  ;; Publications, New York, 1989, p. 60.
  (define corner-bb
    (filled-triangle 0 1 1 1 1 0))
  
  (define rcross-bb
    (overlay (filled-triangle -1/2 1/2 -1/2 -1/2 1/2 -1/2)
             (overlay (overlay (filled-triangle 1/2 -1/2 1 1/2 1/2 1/2)
                               (filled-triangle 1 -1 1/2 -1/2 1 1/2))
                      (overlay (filled-triangle -1/2 1/2 1 1 1 1/2)
                               (filled-triangle -1 1 -1/2 1/2 1 1)))))

  (define quilt
    (lambda (image width height)
      (let ((stacked-image (stack-of-copies height image)))
        (let loop ((n 1)
                   (quilt stacked-image))
          (cond ((< n width) (loop (+ n 1) (side-by-side quilt stacked-image)))
                (else quilt))))))
  (define checkerboard
    (lambda (image width height)
      (let ((stacked-image (stack-of-alternating-copies height image)))
        (let loop ((n 1)
                   (quilt stacked-image))
          (cond ((and (< n width) (even? n)) (loop (+ n 1) (side-by-side quilt stacked-image)))
                ((and (< n width) (odd? n)) (loop (+ n 1) (side-by-side quilt (invert stacked-image))))
                (else quilt))))))))
