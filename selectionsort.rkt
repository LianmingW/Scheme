
;;Selection sort by manipulating integers
;; Return the smallest digit
(define (minimum list)
  (if (zero? (quotient list 10)) list
      (min (remainder list 10) (minimum(quotient list 10)))))


;;Find the index of the digit 
(define (find-index least list index)
  (if (= (remainder list 10) least) index
      (find-index least (quotient list 10) (+ index 1))))

;;Delete the minimum from the orginal integer
(define (delete-min list index)
  (if (= index 0) (quotient list 10)
      (+ (remainder list (expt 10 index)) (* (quotient list (expt 10 (+ index 1))) (expt 10 index)))))


;; Work by insert minimum to the sorted list and delete minimum from the original integer
(define (selectsort list sorted)
  (define (insert element li)
    (+ (* li 10) element))
  (let ((least (minimum list)))
        (if (zero? list) sorted
            (selectsort (delete-min list (find-index least list 0)) (insert least sorted)))))

  
(define (sigma a b)
  (if (> a b) 0
      (+ a (sigma (+ a 1) b))))