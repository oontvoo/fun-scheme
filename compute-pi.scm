;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vy Thao Nguyen
; November 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Computing the digits of `pi`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; matrix 'package'
;
; - ctor: matrix-2x2 <x> <y> <z> <t>
;         where [x  y]
;               [z  t]
;
; - accessors: (a11-of <matrix>) ==> x
;              (a12-of <matrix>) ==> y
;              (a21-of <matrix>) ==> z
;              (a22-of <matrix>) ==> t 
;
; - some matrix-arithmetics: add
;                            multiply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (matrix-2x2 a11 a12 a21 a22)
  (list a11 a12 a21 a22))

(define (a11-of m)
  (list-ref m 0))

(define (a12-of m)
  (list-ref m 1))

(define (a21-of m)
  (list-ref m 2))

(define (a22-of m)
  (list-ref m 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-mtrx <matrix1> <matrix2>)
;
; This performs a matrix addition on
; <matrix1> and <matrix2>
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-mtrx m1 m2)
  (matrix-2x2 (+ (a11-of m1) (a11-of m2))
	      (+ (a12-of m1) (a12-of m2))
	      (+ (a21-of m1) (a21-of m2))
	      (+ (a22-of m1) (a22-of m2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(compose <matrix1> <matrix2>)
;
;This performs a matrix-multiplication on
; <matrix1> and <matrix2>
;
; <m1> x <m2>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compose m1 m2)
  (matrix-2x2 (+ (* (a11-of m1) (a11-of m2))
		 (* (a12-of m1) (a21-of m2)))
	      (+ (* (a11-of m1) (a12-of m2))
		 (* (a12-of m1) (a22-of m2)))
	      (+ (* (a21-of m1) (a11-of m2))
		 (* (a22-of m1) (a21-of m2)))
	      (+ (* (a21-of m1) (a12-of m2))
		 (* (a22-of m1) (a22-of m2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (add-matrix-streams m1 m2)
;
; Adds two streams of matrices
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-matrix-streams m1 m2)
  (stream-map add-mtrx m1 m2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; pi()
; return pi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pi)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; This matrix represents f0
  ; (The first element in the serires/stream)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define matrix-0 (matrix-2x2 1 6 0 3))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; The `delta` value added to each current
  ; value to get the next one
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define matrix-d (matrix-2x2 1 4 0 2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; List (stream) of all matrix-d
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define matrix-d-stream (cons-stream matrix-d
				       matrix-d-stream))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; Input stream to compute pi's digits
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define pi-input-strm (cons-stream matrix-0
				     (add-matrix-streams matrix-d-stream
							 pi-input-strm)))

  (define (compute-pi a strm)
    (define (floor-m-to-num m num)
      (quotient (+ (* (a11-of m) num)
		   (a12-of m))
		(+ (* (a21-of m) num)
		   (a22-of m))))
    
    ((lambda (digit)
       (if (eqv? digit
		 (floor-m-to-num a 4))
	   ; produce
	   (cons-stream digit
			(compute-pi (compose (matrix-2x2 10
							 (* -10 digit)
							 0
							 1)
					     a)
				    strm))
	   ; consume
	   (compute-pi (compose a
				(stream-car strm))
		       (stream-cdr strm))))
     (floor-m-to-num a 3)))
				
  ; start here!
  (compute-pi (matrix-2x2 1 0 0 1)
	      pi-input-strm))

;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (display-h-n strm n)
;  (if (pair? strm)
;      (begin
;	(display "[")
;	(display (stream-car strm))
;	(display "]")
;	(if (> n 1)
;	    (display-h-n (stream-cdr strm)
;			 (- n 1))))))

;(display-h-n (pi) 50)

