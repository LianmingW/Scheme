
;; emulate the simpson's rule


;; iterative version
(define (simpson f a b n)
  (simpson-aux f a b n (/ (- b a) (* n 1.0))))

;; A general recursive sigima function
(define (sum a b term next)
  (cond (( > a b) 0)
        (else (+ (term a) (sum (next a) b term next)))))
(define (nextone x) (+ x 1))
(define (square x) (* x x))
(define (cube x) (* (square x) x))

;; Manipulate the data accordingly
;; *2 for even indices and *4 for odd
;; 1 for start and end
(define (simpson-aux f a b n h)
  (define (term k)
    (cond ((= 0 k) (f a))
          ((= n k) (f (+ a (* k h))))
          ((odd? k) (* 4 (f (+ a (* k h)))))
          ((even? k) (* 2 (f (+ a (* k h)))))))
  (*(/ h 3.0) (sum-iter 0 n term nextone)))

;; iterative sigma
(define (sum-iter a b term next)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

  