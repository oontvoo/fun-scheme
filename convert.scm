;; Vy Thao Nguyen
;; 09/27/2012
;;;;;;;;;;;;;;;;;;;;;;;;;

;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.

(define source (with-input-from-file "units.dat" read-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END LOAD FILE;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; <pair> is ( <kg> | <sec> | <m> . <some number>)
; this find the right place in ret to put <pair>
; BNOT by overriding the value that's aready there
; but adding them together
(define (put-kms pair ret pow)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; (find-sum arg0 arg1 arg2 arg3)
  ; 
  ; This sums up the power of two pairs of the same units
  ; 
  ; Eg., (m -2)^2 and (m 5)^2 ==> (m 6)
  ;
  ; <pair> (<m> | <sec> | <kg> . <some number>)
  ; <ulist> 3 - element list, each element has the same form as <pair>
  ; <i>    index, telling the function which pair in <ulist> to choose
  ; <pow>  exponent
  (define (find-sum pair ulist i pow)
    (+ (* (car (cdr pair))
          pow)
       (cdr (list-ref ulist i))))

  ; determine whether <pair> is kg, m or sec
  (cond ((eqv? 'kg (car pair))
         (list (cons 'kg
                     (find-sum pair ret 0 pow))
               (list-ref ret 1)
               (list-ref ret 2)))
        ((eqv? 'm (car pair))
         (list (list-ref ret 0)
               (cons 'm
                     (find-sum pair ret 1 pow))
               (list-ref ret 2)))
        ((eqv? 'sec (car pair))
         (list (list-ref ret 0)
               (list-ref ret 1)
               (cons 'sec
                     (find-sum pair ret 2 pow))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (kms-of arg0 arg1 arg2 arg3)
;  'k' for 'kg'
;  'm' for 'meter'
;  's' for 'second'
;
;
; This 'adds' all the elements in <ulist> together properly
; , also reorganises the result so that the output is a 3-element list:
; (kg a)(m b)(sec c)
;
; Eg., if ulist = (m 1)(m -4)(kg 2)(s 1)(s 5)
;      then this returns (kg 2)(m -3)(s 6)
;
; <ulist> a list of units, each element has either of these forms:
;         (kg a) (m b), or (sec c)
;         [Where a, b, c are numbers(or expression that evalues to a number)]
; <len> length of the list, used as the counter to signify when to stop
;       TODO: Do not need this, could check for End-of-list instead
; <ret> the 3-element list containing the values computed so far
; <pow> The exponent of the 'parent' unit which got converted to <ulist>
(define (kms-of ulist len ret pow)
  ((lambda (cur)
     (if (> len 1)
         (kms-of (cdr ulist)
                 (- len 1)
                 cur
                 pow)
         cur))
   (put-kms (car ulist)
            ret
            pow)))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; to-SI arg0
;
; Given a unit, return its equivalence KMS
; in this form ( <factor> (kg a) (m b) (sec c))
;
; Eg., (watt 2)(m 2) 
;             --> (1 (kg 2)(m 4)(sec -6)) (m 2)
;             --> (1 (kg 2)(m 6)(sec -6))
;
; <units> a list of pairs, each of which has this form (<name> <exp>)
(define (to-SI units)
  (define (find-SI units ret len)

    ; To be used by factor and unit-list                           
    ; this basically find the associated unit list with
    ; the given unit name                                  
    ; If the given unit name is not found, then return the
    ; designated default value
    ;(look-up <name> (<car> | <cdr>) <default value>)                          
    (define (look-up name f def)
      ((lambda (val func default)
         (if (not val) ; val == #f means unit is not found (or base unit?)
             default
             (func (car (cdr val)))))
       (assoc name source) f def))
   
    ; if <name> is not in the database                 
    ; assume it's the base-unit --> factor == 1                    
    (define (factor name)
      (look-up name car 1))

    ; if <name> is not in the db                        
    ; assumbe it's the base-unit ---> translates to (name 1)    
    ; Eg., (unit-list 'm) --> ((m 1))                  
    (define (unit-list name)
      (look-up name cdr (list (list name 1))))

    ; normalise the <units> list to a list of this form:
    ; (<coeffeicent> (kg a)(m b)(sec c))
    ((lambda (cur units len)
       (if (= len 1)        ; if there's no more unit in the lists to normalise
           cur              ; just the return the list computed so far
           (find-SI (cdr units) ; otherwise, keep computing
                    cur
                    (- len 1))))
     ((lambda (unit ret len) ; normalise the given unit
        (cons (* (expt (factor (car unit))
                       (car (cdr unit)))
                 (car ret)) ; first element is the factor
              (kms-of (unit-list (car unit))
                      (length (unit-list (car unit)))
                      (cdr ret)
                      (car (cdr unit)))))
      (car units)
      ret
      len)
     units
     len))

  ; apply find-SI to convert `units`
  ; all to kg, m, sec
  (find-SI units
           (list 1
                 (cons 'kg 0)
                 (cons 'm 0)
                 (cons 'sec 0))
           (length units)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; convert arg0 arg1
;
; Convert values from one unit to another
;
; Eg.,  (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
;   ==> (0.01023065476190476 (mi 1) (hr -1))
;
; Notes: The order of the units does NOT matter.
; Eg., 
;    (convert '(1 (m 1)(sec -2)) ((mi 1)(hr -2)))
; == (convert '(1 (sec -2)(m 1)) ((mi 1)(hr -1)(hr -1)))
; == (convert '(1 (sec -1)(m 1)(sec -1) ((mi 1)(hr -2))))
; ....
;
; Also, if the units specified are not found in the database, they are
; assumed to be the base unit (which is wrong! but we'll dont deal with that) 
(define (convert from to)
  ; u1 and u2 have the same exponents for kg, m and s
  ; <=>  u1 and u2 are 'convertible' to one another
  (define (compat? u1 u2)
    ((lambda (same-at)
       (and (same-at 1)
            (same-at 2)
            (same-at 3)))
     (lambda (n)
       (= (cdr (list-ref u1 n))
          (cdr (list-ref u2 n))))))

  ; do the conversion
  ; 
  ; a = x (SI) 
  ; b = y (SI)
  ; => a = x * b / y
  ;
  ; In other words, a can be expressed in terms of b, or converted to be as
  ; a = x * b / y
  ((lambda (n si-from si-to)
     (if  (compat? si-from si-to)
          (cons (* (/ (car si-from)
                      (car si-to))
                   n)
                to)
          (error "Incompatible type: <" (cdr from) "> and <" to ">")))
   (car from)
   (to-SI (cdr from))
   (to-SI to)))


;;;;;;;;;;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test convert
; (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))

; 1000  m/s^2  ---> m/hour^2
;(convert '(1000 (m 1)(sec -2)) '((m 1)(hr -2))) 
;(convert '(1000 (m 1)(sec -1)(sec -1)) '((m 1)(hr -2)))

; expect error
;(convert '(1000 (m 2)(sec -1)(sec -1)) '((m 1)(hr -2)))

;(convert '(5 (joule 2)(N -3)(hr 1)) '((kg 2)(m 4)(sec -4)(dyn -3)(sec 1)))

;(convert (list 27.5 (list 'furlong 1) (list 'fortnight -1))
;         (list (list 'mi 1) (list 'hr -1)))

;;;;;;;;;;;;;;;;;;;;; END TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
