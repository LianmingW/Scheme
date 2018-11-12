

; Fifth Homework Set
; CSc 335
; Fall 2018

;eq? check memory location
;= only works for numbers
;eqv? check primitive values, stronger than eq
;equal? strongest, can be used to list vectors etc.

;(define lat?
  ;(lambda (lst)
   ; (cond ((null? lst) #t)
    ;      (else 
     ;      (and
     ;       (atom? (car lst)) 
      ;      (lat? (cdr lst)))))))
; Pre: lst is a list
; Post: if list contains only atoms #t, else #f
;Proof:
;If the length of the list is 0, we know it's a empty list, so it's a lat
;Else lat? will take in the cdr of lst, as we know each cdr would reduce the length by 1, eventually we will have an bunch of ands of (car lst) (assuming the recursive process works correctly)
;, which is each elts of lst and a #t(by null? lst)
;if any of them is false, the result will be false, so the post condition is indeed lat?, that if the list contains only atoms or is empty.

;Prove by stating that assume (lat?(cdr lst)) works correctly
;If car lst is not atom, because of the and gates, the result is false, else, lat? is true only if the remaining of the lst contains all atoms, and this is what lat? lst should output

;firsts, return a list of first element of the lists
;lists should be in the format ( L1 L2 L3 L4 ...), each lists must contain at least one elements
;note caar is equavelent to (car (car lsts))
(define (firsts lsts)
  (if (null? lsts) '()
      (cons (caar lsts) (firsts (cdr lsts)))))
;(firsts '( (1 2 3) ( 2 2 3) (5 6 2)))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework Problems

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; 1.  Prove the correctness of the value function presented in lecture9.scm.  Your argument should be an
; induction on (the complexity of) the argument.  State clearly what assumptions you make.

;(define value
 ; (lambda (aexp)
  ;  (cond ((natnum? aexp) aexp)
  ;        (else (cond ((plus-aexp? aexp) (plus (value (first-operand aexp))
   ;                                            (value (second-operand aexp))))
     ;                 ((times-aexp? aexp) (times (value (first-operand aexp))
      ;                                           (value (second-operand aexp))))
      ;                ((power-aexp? aexp) (expon (value (first-operand aexp))
     ;                                            (value (second-operand aexp))))
    ;                  )))))

;Pre: aexp is a set of i-aexp (1 + 2) (2 * 3) (2 * (3 + 4)) etc
;Post: the value of the i-aexp is returned;
;Base case: if the aexp is a natrual number, then the number is returned.
;Induction Hypothesis: Assume that value x of all x 's that is simpler than the original aexp is computed correctly.
;Proof:
;If the axep is natural number, we return that number, if it's not, and using the defination plus-aexp, times ...., we seperated the condition into 3 parts;
;If it's a + in the middle of the experation, we add the first operands of the aexp and the second-operand of aexp, because first and second operand are both
;simpler than our original aexp, they should both return the corresponding result we desired, and if we add both of them together, we have the desired value aexp returned.
;Similar for times and power.






; 2.  Make all changes needed to allow the a-iexp calculator to work with numbers given in base 1.
; Explain carefully how you will represent such numbers; design plus, times, and exponent procedures
; for them.  Prove correctness of your functions.

;unary number: 1 =1, 11=2, 111 =3
;for the value function to work with base1 , whenever we see the aexp is a natural number(eg 1 11 111...), we quotient the giving aexp by 1 until 0 is returned, and add 1's in that process
;finally returns the numeric value of that unary number, so all we need is an additional function, unary value


;; if quotient num 10 is 0, this means num is 1, so we return the last 1 needed to be added
(define (unary num)
  (if (= (quotient num 10) 0) 1
      (+ 1 (unary (quotient num 10)))))

;(unary 1111111)
;proofs are just similar to above

; so we just replace((natnum? aexp) aexp) with ((natnum? aexp) (unary num)) and it should work correctly



; 3.  Exercises 2.2, 2.3 and 2.4 in Abelson and Sussman.  Proofs are not necessary.
;2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; point in the form (x y)
;return x
(define (x-point p)
  (car p))
;return y
(define (y-point p)
  (cdr p))
;cons x y
(define (make-point x y)
  (cons x y))
;sample points
(define sp (make-point 1 2))

(define ep (make-point 3 4))
;cons two points
(define (make-segment p1 p2)
  (cons p1 p2))
;return start point
(define (start-segment sg)
  (car sg))

(define (end-segment sg)
  (cdr sg))
;sample segment
(define sg (make-segment sp ep))
;return makepoint using the average value of start, end point's x value and y value, which is mid point of the segment
(define (midpoint sg)
  (make-point (/(+ (x-point(start-segment sg)) (x-point(end-segment sg)))2) (/(+ (y-point(start-segment sg)) (y-point(end-segment sg)))2) ))
;sample data
(print-point sp)
(print-point ep)
(print-point (midpoint sg))

;2.3
;rectangle structure
;we just need 2 points to construct a rectangle: upper-left point, and lower-right point (or upper-right lower-left if you want)
;example
;  Upperleft
;  (1,1)------------|
;    |              |
;    |              |
;    |------------(3,-1)
;                lowerright

(newline)


(define upper-left (make-point 1 1))
(define lower-right (make-point 3 -1))

;We need to make sure that upper-left and lower-right property are satisfied, or all functions will fail
(define (rectangle upper-left lower-right)
  (cond ((and (< (x-point upper-left) (x-point lower-right)) (< (y-point lower-right) (y-point upper-left)))
  (cons upper-left lower-right))
  (else (display "invalid input!"))))

(define (get-upper-left rect)
  (car rect))
(define (get-lower-right rect)
  (cdr rect))

(define (rect-length rect)
  (- (x-point (get-lower-right rect)) (x-point (get-upper-left rect))))
(define (rect-width rect)
  (- (y-point (get-upper-left rect)) (y-point (get-lower-right rect))))

; 2 * (w+l)
(define (perimeter rect)
  (* 2 (+ (rect-length rect) (rect-width rect))))

(define (area rect)
  (* (rect-length rect) (rect-width rect)))

(define r (rectangle upper-left lower-right))
r
(rect-length r)
(rect-width r)
(perimeter r)
(area r)


;second representation
;a point and the length and width of the rectangle

(define (rectangle-v2 upper-left length1 width)
  (cond ((and (> length1 0) ( > width 0))
  (cons upper-left (make-point (+ (x-point upper-left) length1) (- (y-point upper-left) width))))
  (else (display "invalid input!"))))

(define r2 (rectangle-v2 upper-left 5 3))
r2
(perimeter r2)
(area r2)


;2.4
(define (cons x y)
 (lambda (m) (m x y)))
(define (car z)
 (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))



;proof
;(cdr (cons 1 2))
;(cdr (lambda (m) (m 1 2)))
;(lambda (m) (m 1 2)) lambda (p q) q)))
;substute m with (lambda (p q) q))
;(lambda((p q) q) 1 2)))
; 2






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

