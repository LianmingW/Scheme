
; Problem 1 (20 points) Write a certify an iterative program, using lists, named
; nth-row-of-pascal-triangle which inputs a positive integer n and which outputs
; the nth row of the pascal triangle,

; Thus (nth-row-of-pascal-triangle 3) returns the list (1 2 1), and
; (nth-row-of-pascal-triangle 4) returns (1 3 3 1).  Your program should of course
; work for other positive integers n -- for example, (nth-row-of-pascal-triangle 10)
; should return (1 9 36 84 126 126 84 36 9 1)

; (The main program should be iterative, but you are free to use properly recursive
; auxiliary functions if it seems useful to do so.)

; Be sure to include (WORKING) tests of your function.  

;;;; INSERT YOUR ANSWER HERE

(define (pascal row col)
  (cond ((= row col) 1)
        ((= row 1 ) 1)
        ((= col 1) 1)
        (else (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1))))))

(define (enumerate-interval start end)
  (cond ((> start end) '())
        (else (cons start (enumerate-interval (+ start 1) end)))))

(define (pascal-row row)
  (define (aux col result)
    (cond ((> col row) result)
          (else (aux (+ col 1) (append result (list(pascal row col)))))))
  (aux 1 '()))
;;Invariant result represent the row of the pascal triangle truncating (row-col) number of elements)
;;Assumption: Pascal returns correct value
;;Base case: if row is 1, we simply return 1 as after the first iteration,  col > row, we returned the result that just have 1, and that is correct
;;Else, we insert the corresponding pascal value of given row and col, as row never changes, and pascal return correct value, in each moment, result represent the incomplete pascal row missing row-col  number of elements.
;;And after col > row, we return the result, where we miss no elements.


(display "question 1")
(newline)
(pascal-row 4)
(pascal-row 6)
(pascal-row 10)
; Problem 2a (10 points)

; We defined accumulate

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))


; Use this function to implement the function mymap so that a call (mymap p seq), where p is a procedure and seq is a
; list, returns the same value as (map p seq), where map is as defined in Scheme.

; No proof is necessary, but I do expect you to include (WORKING) test cases. 


;;;; INSERT YOUR ANSWER HERE
(display "question 2a")
(newline)
(define (mymap p seq)
  (if (null? seq) '()
      (accumulate (lambda(x y) (cons (p x) y)) '() seq)))

(define sample-list '(1 2 3 4 5))
(map odd? sample-list)
(mymap odd? sample-list)
(map (lambda (i) (* i i)) sample-list)
(mymap (lambda (i) (* i i)) sample-list)


; Problem 2b (10 points)
; Implement a function count-leaves using accumulate and map, so that for any list t,
; (count-leaves t) returns the number of leaves of t (thinking of t as a tree).

; No proof is necessary, but I do expect you to include (WORKING) test cases.


(display "question 2b")
(newline)
;;;; INSERT YOUR ANSWER HERE
(define (leave? input)
  (cond ((null? input) 0)
        ((pair? input) (+ (leave? (car input)) (leave? (cdr input))))
        (1)))

(define (count-leaves t)
  (cond ((null? t) 0)
        ((accumulate + 0 (mymap leave? t)))))
(mymap leave? '(1 2 3 4 ( 4 5 6 ) (9 2 3 )5))

(count-leaves  '(1 2 3 4 ( 4 5 6 ) (9 2 3 )5))




; Problem 3a (40 points)

; As you recall, any proposition constructed with the connectives ~ (not), ^ (and), v (or) and => (implies) can be transformed
; to a logically equivalent proposition constructed using only ~ and v.  The main equivalences used are De Morgan's law, namely
; (a ^ b) = ~ (~a v ~b), and p => q = ~p v q.

; In this problem you are asked to define a datatype (with suitable representations) of propositions, and to use the resulting
; collection of constructors, selectors and classifiers to write and prove a procedure normalize which inputs a proposition using
; ~, ^, v, and => and which returns a logically equivalent proposition which uses only v and ~.

; For example, if e is the representation of ~ (a ^ b), (normalize e) will return the representation of ~a v ~b.

; As an indication of what I have in mind for the proposition datatype, e would be built as (make-not (make-and 'a 'b)).

; The procedure normalize should progress by recursive descent; your proof should be inductive -- be sure to say what you
; are inducting on.

; Be sure to include (WORKING) test cases.  



;;;; INSERT YOUR ANSWER HERE
(display "question 3a")
(newline)
(define (make-not a)
  (list '~  a))
(define (make-and a b)
  (list a '^ b))
(define (make-or a b)
  (list a 'v b))
(define (make-imply a b)
  (list a '=> b))
; gave up
;(define (normalize e)
;  (cond ((null? e) e)
  ;      ((not (pair? (car e))) e)
     ;   ((pair? (car e)) (cons (normalize (car e)) (normalize (cdr e))))
    ; ;   ((and (not (pair? (cadr e))) (not (eq? (cadr e) '~))(not(null? (cddr e)))) (cons (normalize (car e)) (cons (cadr e) (normalize (caddr e)))))
    ;    ((not (pair? (cadr e))) e)
      ;  

; Problem 3b (10 points - Extra Credit)

; Write a function remove-double-not which works with your representation of propositions to remove double negations.  For
; example, if ~ ~ ((~ x) v (~ y)) is represented as (~ (~ ((~ x) v (~ y)))), then
; (remove-double-not '(~ (~ ((~ x) v (~ y))))) returns ((~ x) v (~ y)).

; Note that double negations may occur at arbitrary depth, so that a recursive routine is necessary.  Test your procedure
; by composing it with normalize -- what output do you get for your representation of ~ (a ^ b)?  

; You do not need to give a proof.  You do need to show that your code works on some well-chosen test cases. 


(define x (make-not 1))
(define y (make-and 3 4))
(define z (make-or 5 6))

(define v (make-and x y))
(display "question 3b")
(newline)
(define (remove-double-not e)
  (cond ((null? e) e)
        ((not (pair? e)) e)
        ((null? (car e)) e)
        ((pair? (car e)) (cons (remove-double-not (car e)) (remove-double-not (cdr e))))
        ((and (not (pair? (cadr e))) (not (eq? (cadr e) '~))(not(null? (cddr e)))) (cons (remove-double-not (car e)) (cons (cadr e) (remove-double-not (caddr e)))))
        ((not (pair? (cadr e))) e) ; case of (~ 1)
        ((eq? (car e) '~) (if (eq? (caadr e) '~) (remove-double-not (cadadr e)) (cons (car e)(remove-double-not (cdr e)))))
        (else (cons (car e)(remove-double-not (cdr e))))))

(define no (make-not x))
(define no2 (make-not no))
(define no3 (make-not(make-not (make-not(make-not no2)))))
(define sample (make-not '(~ (~ ((~ x) v (~ y))))))
(define sample2 '(~ (~ (~ ((~ x) v (~ (~ (~ y))))))))
no3
(remove-double-not no3)
sample
(remove-double-not sample)
sample2
(remove-double-not sample2)


(define sample3 (make-and sample2 sample2))
sample3
(remove-double-not sample3)
; Problem 4a.  (10 points) Write and prove correct a pair of MUTUALLY RECURSIVE programs odd-indexed-elements and
; even-indexed-elements to produce from one list L the two lists consisting, respectively, of the elements of L with
; even index and the elements of L with odd index.  For example, (odd-indexed-elements '(a b c d e f g h)) = '(b d f h)
; and (even-indexed-elements '(a b c d e f g h)) = '(a c e g).  (Hint: use one proof for both functions).  

(display "question 4a")
(newline)
(define (odd-indexed lst)
  (if (null? lst) '()
      (even-indexed (cdr lst))))

(define (even-indexed lst)
  (if (null? lst) '()
      (cons (car lst) (odd-indexed (cdr lst)))))

  

(define s2 '(0 1 2 3 4 5 6 7 8 9))
(odd-indexed s2)
(even-indexed s2)





; 4b.  (15 points) Write and prove correct a procedure mergesort which inputs a single (unsorted) list of numbers
; and which outputs the list consisting of the elements of the input list, in sorted order. 
; Your procedure should make use of your solution to 4a.

; (HINT: start by writing a procedure merge which takes two lists of numbers, with each list sorted in order from
; smallest to largest, and which produces a third list containing all the (distinct) elements of the original lists,
; in sorted order.)


; Again, for both problems, I expect you to give (working) test cases.  

(display "question 4b")
(newline)
(define (merge-lists lst lst2)
  (cond ((null? lst) lst2)
        ((null? lst2) lst)
        (else (if (< (car lst)(car lst2))
                  (cons (car lst) (merge-lists (cdr lst) lst2))
                  (cons (car lst2) (merge-lists lst (cdr lst2)))))))
; Pre lst lst2 are sorted lst with increasing order
; Post: sorted list merging lst lst2 is returned
;Proof:
;Base case: if lst or lst2 is null, we return the corresponding lst
;Hypothesis: merge-lists works for simpler lst than lst and lst2
;Induction: if not base case, we compare the first element of the two lsts, and put cons the smaller elements into the stack
;by our hypothesis, merge-lists should return a sorted lst merging (cdr lst) lst2, as we know (car lst) must be smaller than all the element in (cdr lst) by the pre-condition,
;and car lst is smaller than the first element of lst2, we are guranteed we insert the smallest element in the front of our result.
;Similar when car lst2 is smaller. So the sorted list ordered from small to large is returned

(define (merge-sort lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        (else (merge-lists (merge-sort (odd-indexed lst)) (merge-sort (even-indexed lst))))))



(merge-sort '( 1 6 2 6 6 2 4 7 7 8 12 2 0 2 -5))



;Proof
;Pre: a list of int is inputed
;post : a sorted list is returned
;Base case: if lst is null or just containing an atom, null or the lst containing the atom is returned
;Induction Hypothesis: Assume merge-sort works for shorter length lsts than original LST
;Case: null or atom, base case return;
;Else; we merge the odd and even indexed merge-sort
;By the induction hypothesis, merge-sort odd and even returns sorted odd and even;
;As merge-lists return a sorted lists if the two lsts input is sorted, the merge-sort returns a sorted lst.




