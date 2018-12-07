;; Topic: Class object container.

;; Basic data struture: a list of lists where the first item  in the list is the structure of the class object, all the following class object must have same length as the
;; structure (same number of variables and must have a name at least). We search through the class by matching the name.
;; Application: Imitate the object oriented programming class data structure. We could pull out class object and pass it to functions just like we did in oop.


(define (build structure)
  (cond ((null? structure) (display "Please enter the class structure"))
        ((not (pair? structure)) (display "Please enter a list!"))
        (else (list structure))))
;; Initialize a new object

(define (fill_object object len)
  (cond ((= (length object) len) object)
        (else (fill_object (append object (list 'null)) len))))
  
(define (new class new_object)
  (cond ((null? new_object) (display "Enter at minimum name of the new object!"))
        ((= (length (car class)) (length  new_object)) new_object)
        (else (fill_object new_object (length (car class))))))

(define (insert class object)
  (set! class (append class (list (new class object)))) class)

(define (get_structure class)
  (car class))


(define rectangle (build '(name length width)))

(insert rectangle '(rect1))
(insert rectangle '(rect2 10 8))

(define x 3)

(define (foo)
  (define x 4)
  x)

(define (bar)
  (set! x 4) x)
  



