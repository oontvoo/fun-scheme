; part 4.0
; (mult-stream <multiplier> <stream of digits>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mult-stream m strm)
  (define (do-mult-stream a a-list pow input-strm m)
    (define (new-a-list a old-len)
      (define (num->list num)
	(define (do-to-list num)
	  (if (> num 0)
	      (cons (remainder num 10)
		    (do-to-list (quotient num 10)))))
	(reverse (do-to-list num)))
      (define (pad-0 ret this-len old-len)
	(if (>= old-len this-len)
	    (pad-0 (cons 0 ret)
		   (+ this-len 1)
		   old-len)
	    ret))
      ; pad the 0 to the left if necessary
      ((lambda (new-list)
	 (pad-0 new-list
		(length new-list)
		old-len))
       (num->list a)))

    (define new-pow
      (lambda (ret cur-pow power)
	(if (< cur-pow power)
	    (new-pow (* 10 ret)
		     (+ 1 cur-pow)
		     power)
	    ret)))


    ; main 'body' of do-mul-stream
    (if (null? input-strm)
	a-list
	(if (and (not (null? a-list))
		 (< (+ m (remainder a pow))
		    pow))
	    ;;produce                                                            
	    (cons-stream (stream-car a-list)
			 (do-mult-stream (remainder a pow)
					 (stream-cdr a-list)
					 (quotient pow 10)
					 input-strm
					 m))
	    ;;consume
	    ((lambda (new-a)
	       ((lambda (new-a-list)
		  (do-mult-stream new-a
				  new-a-list
				  (new-pow 1
					   0
					   (- (length new-a-list) 1))
				  (stream-cdr input-strm)
				  m))
		(new-a-list new-a
			    (length a-list))))
	     (+ (* a 10)
		(* (stream-car input-strm)
		   m))))))
  
  ; start from the left 
  (do-mult-stream 0 '() 0 strm m))


;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;
;(display-n (mult-stream 7 '(1 9 9 9 3)) 6)
;(display-n (mult-stream 9 '(9 9 9 9 9)) 7)
;(display-n (mult-stream 10 '(1 0 9 9)) 6)
;(display-n (mult-stream 2 '(5 5 5)) 4)
