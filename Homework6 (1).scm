; Sixth Homework Set
; CSc 335
; Fall 2018

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; First Problem

; replace-nth

; Here is a tree recursion somewhat more complicated than those we have looked at until now


; develop and certify a scheme program replace-nth which takes as input

;        a list lst, not necessarily a list of atoms
;        a positive integer, n
;        an atom, old
;        an atom, new

; (replace-nth lst n old new) should replace the nth occurrence of old in 
; lst by new (and leave everything else unchanged)

(define (replace-nth lst n old new)
  (define (aux lst n result)
    (cond ((null? lst) result)
          ((< n 0) '())
          ((eq? old (car lst)) (if (= n 0) (aux (cdr lst) (- n 1) (append result (list new))) (aux (cdr lst) (- n 1) (append result (list (car lst))))))
          ((pair? (car lst)) (aux (cdr lst) n (append result (list (aux (car lst) n result)))))
          (else (aux (cdr lst) n (append result (list (car lst)))))))
  (aux lst n '()))



(define L1 (list 1 2))
(define L2 (list 3 4))
(define L3  (list L1 (list L1 L2)))
(define L4 (list L3 L2))
L4





















; Additional Problems

; Abelson and Sussman, Exercise 2.27

;Exercise 2.27. Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
;procedure that takes a list as argument and returns as its value the list with its elements reversed and
;with all sublists deep-reversed as well. For example,
;(define x (list (list 1 2) (list 3 4)))
;x
;((1 2) (3 4))
;(reverse x)
;((3 4) (1 2))
;(deep-reverse x)
;((4 3) (2 1))

;Because cons always insert item infront of a list, by inserting the front item in the original lst first, we reversed the lst
(define (reverse lst)
  (define (aux lst result)
    (if (null? lst) result
        (aux (cdr lst) (cons (car lst) result))))
  (aux lst '()))

(reverse L4)

;If lst is null, we return lst, else if car lst is not atom, we append the deep-reverse of that sub-tree to the cdr of original lst, we add list infront as we lost one pair of () by car the lst, we want to perserve that
;if car lst is an atom, simply insert that to the back of the cdr of original lst
(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((pair? (car lst)) (append (deep-reverse (cdr lst))  (list(deep-reverse (car lst)))))
        (else (append (deep-reverse (cdr lst)) (list(car lst))))))
(deep-reverse L4)


;Another version of deep-reverse utilizing our reverse function
;Using the aux function with result intiallized to null, if lst is null, we simply return null,
;Else if the first element of the lst is a lst, we return the reversed cons sub-list and the list remain
;This is because that assuming aux works for any lst simpler than our LST, the original lst, (cons (aux (car lst)) (aux (cdr lst))) should return the correct deep-reversed sub-list,
;what we need to do is to reverse the order of the cons so the entire lst is reversed (put the cdr infront of car, because cons insert car infront)(we don't want to insert cdr into car as it would generate tons of ())('s
;If the car lst is just an atom, simply insert that to the front of result
(define (deep-reverse-v2 lst)
  (define (aux lst result)
    (cond ((null? lst) result)
          ((pair? (car lst)) (reverse (cons (aux (car lst) result ) (aux (cdr lst) result))))
          (else (aux (cdr lst) (cons (car lst) result)))))
  (aux lst '()))

(deep-reverse-v2 L4)





; Abelson and Sussman, Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;a.
; we need cadr mobile to get the right branch as
; a mobile have a structure of ((left) (right))
; if we just cdr, we would have ((right)), which is not what we desired
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;b.
;based on https://github.com/jimweirich/sicp-study/blob/master/scheme/chapter2/ex2_29.scm
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
  (if (number? structure) structure (total-weight structure))))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define left (make-branch 1 1))
(define right (make-branch 2 2))
(define mobile (make-mobile left right))

mobile
(total-weight mobile)
;c
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))
(define (balanced? mobile)
  (= (torque (left-branch mobile)) (torque (right-branch mobile))))

(balanced? mobile)
(define new-left (make-branch 2 2))
(define new-mobile (make-mobile new-left right))
(balanced? new-mobile)



;d
(define left-bran (cons 1 2))
(define right-bran (cons 3 4))
(define con-mobile (cons left-bran right-bran))
; As cons simply insert the left-bran into right-bran, a new list is not made (no extra ()), so just car and cdr is enough


; Abelson and Sussman, Exercise 2.32

(define (subsets s)
  (if (null? s)
      '()
      (let ((rest (subsets (cdr s))))
        (append rest (map car rest)))))


(subsets '(1 2 3))





; Abelson and Sussman, Exercise 2.37
; Abelson and Sussman, Exercise 2.41
; Abelson and Sussman, Exercise 2.42







