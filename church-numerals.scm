;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Vy Thao Nguyen
; October, 2012
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Church numerals
; Define church numerals
;-----------

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))
; .... up till 9 ....


;---------
; Convert a church numeral to regular integer 
(define (church->int church)
  ((church 
    (lambda (a) (+ a 1)))
   0)

;;;; tests converting churchh num to int
(display "converting church to int")

(church->int one) ; display 1
(church->int two) ; display 2
(church->int three) ; display 3
; ...


