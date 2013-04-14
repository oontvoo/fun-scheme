; Vy Thao Nguyen
; (display-n <stream> <n> )
; Display the first n elements in the stream

(define display-n
  (lambda (stream n)
    (if (pair? stream) ; if it' a stream to begin with
        (begin
          (display (stream-car stream))
          (display #\newline)
          (if (> n 1)
              (display-n (stream-cdr stream)
                         (- n 1))))
        (display #\newline))))

;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;
(define (ints-from n)
  (cons-stream n (ints-from (+ n 1))))
(define ints (ints-from 0))

; display 10 ints
(display-n ints 10)

; display empty stream
(display-n the-empty-stream 10)
        
          
