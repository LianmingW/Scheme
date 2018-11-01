



;; recursive version
(define (insertion-sort remain sorted)
  (cond ((= remain 0) sorted)
        ((< remain 10) (insert sorted remain 0))
        (else(insertion-sort (quotient remain 10) (insert sorted (remainder remain 10) 0)))))
;; Using quotient and remainder to go through the integer and compare the input with each of them and put the input in appropriate position
(define (insert sorted x counter )
    (cond ((= sorted 0) x)
          ((< sorted 10) (if (< x sorted) (+ (* x 10) sorted) (+ (* sorted 10) x)))
          ((< x (remainder (quotient sorted (expt 10 counter)) 10)) (insert sorted x (+ counter 1)))
          (else (+ (* (quotient sorted (expt 10 counter)) (expt 10 (+ counter 1))) (remainder sorted (expt 10 counter)) (* x (expt 10 counter))))))
;; General version
(define (insertion-sort-g remain sorted term)
  (cond ((= remain 0) sorted)
        ((< remain 10) (insert-g sorted remain 0 term))
        (else(insertion-sort-g (quotient remain 10) (insert-g sorted (remainder remain 10) 0 term) term))))

(define (insert-g sorted x counter term)
    (cond ((= sorted 0) x)
          ((< sorted 10) (if (term x sorted) (+ (* x 10) sorted) (+ (* sorted 10) x)))
          ((term x (remainder (quotient sorted (expt 10 counter)) 10)) (insert-g sorted x (+ counter 1) term))
          (else (+ (* (quotient sorted (expt 10 counter)) (expt 10 (+ counter 1))) (remainder sorted (expt 10 counter)) (* x (expt 10 counter))))))


;; Pascal Triangle
(define (pascal row col)
  (cond ((= col 1) 1)
        ((= row col) 1)
        ((= row 1) 1)
        (else ( + (pascal(- row 1) (- col 1)) (pascal (- row 1) col )))))


;; Returns the value of nth term of a binomial expansion with help of pascal triangle (get the prefix)
(define (binomial n)
  (lambda (k a b) (* (pascal (+ n 1) k) (expt a k) (expt b (- n k)))))