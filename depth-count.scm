;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vy Thao Nguyen
; Compute the depth of a stream
;
; For eg., ((1, (2, 3)), 3, 4, 5)
;       => Depth = 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dept-count strm)
  (define (do-count str n)
    (if (null? str)
	n
	(if (pair? str)
	    ((lambda (left right)
	       (if (< left right)
		   right
		   left))
	     (do-count (car str) (+ n 1))
	     (do-count (cdr str) (+ n 1)))
	    n)))
  (do-count strm 0))



(apply dept-count 
       '(( ((((3))))
	  2
	  4
	  7) ))


