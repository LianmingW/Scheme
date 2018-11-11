
; Fourth Homework Set
; CSc 335
; Fall 2018


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Homework4.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are some homework problems to get you started with lists

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that I have sometimes deliberately offered incomplete specifications - if you find this
; to be the case, you will need to complete the specification as you deem best.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Give both recursive and iterative procedures (along with their arguments) for each

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Write your own version of length using the list functions we have discussed.  You can find
; length documented at http://www.schemers.org/Documents/Standards/R5RS/

; The idea is suggested by (my-length '(a b c d)) = 4.

;recursive
;recursively add 1 until we cdr done the list to null
(define (my-lengt lst)
  (if (null? lst) 0 (+ 1 (my-lengt (cdr lst)))))


;iterative version
;use counter to store the information
(define (my-length lst)
  (define (aux lst counter)
    (if (null? lst) counter (aux (cdr lst) (+ counter 1))))
  (aux lst 0))


; 2.  Write your own version of list-ref using the list functions we have discussed.  You can find
; list-ref documented at http://www.schemers.org/Documents/Standards/R5RS/

; Briefly, the idea is indicated by this example:  (my-list-ref '(a b c d) 2) = c.  Note the 0-based
; indexing.  What happens if the input index exceeds the size of the input list?
; we don't care about wrong user input, we just care about functionality

;recursive
;stack cdr until we reach the index, and return the car of the lst remain
(define (my-list-re lst index)
  (define (aux lst index)
    (if (zero? index) lst
        (cdr (aux lst (- index 1)))))
  (car (aux lst index)))


;iterative
;if index reach 0 we return the car of lst, else we call the function again and cdr down the lst, iterative as the stack stores the newest lst
(define (my-list-ref lst index)
  (if (zero? index) (car lst)
      (my-list-ref (cdr lst) (- index 1))))



; 3. Write a function start that takes two arguments, lst and num, and which returns the
; first num elements of lst.

;recursive version
;stack cons of the first element of the lst, and cdr down the lst in the process, so each time we cons the first element of the lst until num is zero
;example lst = '(1 2 3 4) num = 2
;first:
;(cons 1 (first-num '(2 3 4) 1))
;next:
;(cons 1 (cons 2 (first num '(3 4) 0))
;finally
;(cons 1 (cons 2 '())
;as cons always insert elt to the front we have '(1 2) as result

(define (first-num lst num)
  (if (zero? num) '()
      (cons (car lst) (first-num (cdr lst) (- num 1)))))
;(first-num '(1 2 3 4 5) 3)

;iterative version
;because cons always insert the elt into the front of the result, we user append to insert the elt to the back of the lst to maintain right order
(define (first-num-i lst num)
  (define (aux lst num result)
    (if (zero? num) result
        (aux (cdr lst) (- num 1) (append result (list (car lst))))))
  (aux lst num '()))

;(first-num-i '(1 2 3 4 5) 3)

 

; 4.  Write a function but-last that takes two arguments, lst and num, and which returns the
; list of all but the last num elements of lst.

;recursive
;similar to question3, but this time we utilize the length function provided by scheme
;we just use the problem3 function but now the number of time is not num but length of the list - num, so we excluded the last num elts of the lst
;example '(1 2 3 4 5) have length of 5, num = 2, so we return the first 3 elts of the lst, which is correct
(define (but-last lst num)
  (define (aux lst i)
    (if (zero? i) '()
        (cons (car lst) (aux (cdr lst) (- i 1)))))
  (aux lst (- (length lst) num)))
  
;(but-last '(1 2 3 4 5) 3)

;iterative similar manner
(define (but-last-i lst num)
  (define (aux lst i result)
    (if (zero? i) result
        (aux (cdr lst) (- i 1) (append result (list (car lst))))))
  (aux lst (- (length lst) num) '()))

;(but-last-i '(1 2 3 4 5) 3)



; 5.  Write a function end that takes two arguments, lst and num, and returns the last num
; elements of lst.

;recursive
;stack up cdrs until we reach last nums of elt, and return the lst, so the cdrs start to compute
;the times of cdrs is the length of the lst - num, because we want cdr all the element before the last num elts


(define (last lst num)
  (define (aux lst num)
    (if (zero? num) lst
        (cdr (aux lst (- num 1)))))
  (aux lst (- (length lst) num)))

;(last '(1 2 3 4 5) 3)



;iterative
;cdr down the lst until we reach the last num of elt
(define (last-i lst num)
  (define (aux lst i)
    (if (zero? i) lst
        (aux (cdr lst) (- i 1))))
  (aux lst (- (length lst) num)))
;(last-i '(1 2 3 4 5) 2)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; suggested reading:

;;    http://en.wikipedia.org/wiki/Scheme_(programming_language)

