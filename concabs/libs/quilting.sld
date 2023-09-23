(define-library (concabs quilting)
  (export test-bb
	  nova-bb
	  bitw-bb
	  rcross-bb
	  corner-bb)
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
    (overlay (overlay (filled-triangle -1/2 -1/2 1/2 -1/2 -1/2 1/2) corner-bb-new)
             (overlay (overlay (filled-triangle 1 -1 1/2 -1/2 1 0)
                               (filled-triangle 1/2 -1/2 1/2 1/2 1 0))
                      (overlay (filled-triangle -1 1 -1/2 1/2 0 1)
                               (filled-triangle -1/2 1/2 0 1 1/2 1/2)))))))
