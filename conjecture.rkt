 
;; Test the conjecture if sigma(x)^2 = sigma (x^3)

(define (conjecture n)
  (define (square input)
    (* input input))
  (define (cube input)
    (* (* input input) input))
  (define (conjecture-iter counter sum-int cube-sum)
    (cond ((> counter n) (if (= (square sum-int) cube-sum) #t #f))
          (else (conjecture-iter (+ counter 1) (+ counter sum-int) (+ cube-sum (cube counter))))))
  (if (= n 0) #t
      (conjecture-iter 1 0 0)))




