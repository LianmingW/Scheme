
;; Pre: x-> the value of root node of the Tree, r-> the rank of the Tree, c-> the children the Tree have, if any are inputed
;; Post: a Tree with the given parameters is returned
(define(Tree x r c)
  (list x r c))

;; Post: empty-heap is returned 
(define (empty-heap)
  '())

;; Methods to access the components
;; Pre: A correct Tree is inputed
;; Post: Corresponding component of the Tree is returned
(define (root tree)
  (car Tree))
  
(define (rank tree)
  (cadr Tree))

(define (child tree)
  (caddr Tree))

;; Pre: A heap is inputed
;; Post: A boolean value that correctly indicates if the heap is empty or not is returned
(define (empty? heap)
  (if (null? heap) #t #f))


;; Pre: Two Trees with same rank are inserted
;; Post: The merged Tree that satisfied the binomial heap criteria is returned
;; Basic Idea: compare the root value of the two Trees, the Tree with smaller value would become the parent of the other Tree, by inserting other Tree in the front of the child list of smaller Tree
(define (mergeTree n1 n2)
  (let ((root1 (root n1)) (root2 (root n2)) (ranking (rank n1)) (child1 (child n1)) (child2 (child n2)))
    (cond ((< root1 root2) (Tree root1 (+ ranking 1) (cons n2 child1)))
          (else (Tree root2 (+ ranking  1) (cons n1 child2))))))

;; Pre: A tree along with the heap we want to insert the tree into are inputed, note that the input tree rank should not be higher than the first tree of the destination heap,
;; unless the destination heap is empty, this would be guranteed by the way insTree is called.
;; Post: Heap with the new tree inserted is returned
;; Basic Idea: if the heap is empty, we just make the input tree a heap by inserting it to empty list
;; else we extract the left most tree of the heap-> t2, because of the nature of the binomial heap we are implementing, the left most tree is always the tree with the smallest rank
;; (note that mergeTree is for Tree only, not for heap. For a Tree, the left most component is largest rank child),  if the input tree is smaller than t2, its smaller than
;; all the trees in the heap, thus we simply insert it to the left of the heap.
;; Else: the tree must be have the same rank as the t2, we use mergeTree method to merge the two tree and compared the merged tree with next tree in the heap, if the merged tree reach
;; same rank of the next tree in the heap, simply merge them again, else the result heap is reached, we put (cons t1 heap) on the stack, and the stacked up (cons t1 heap) would rebuild
;; the entire heap
(define (insTree t1 heap)
  (cond ((empty? heap) (list t1))
        (else (let ((t2 (car heap)))
                (if (< (rank t1) (rank t2)) (cons t1 heap) (insTree (mergeTree t1 t2) (cdr heap)))))))
;; Pre: a number and destination heap is inputed
;; Post: heap with elt inserted in returned
;; Basic idea: represent the elt as a 0 rank tree and insert that to our heap. As 0 is the lowest rank possible for a tree,
;; insTree pre-condition would not be violated
(define (insert elt heap)
  (insTree (Tree elt 0 '()) heap))

;; Pre: Two heaps are inputed
;; Post: a merged version of the two heap is returned
;; This merge function is just like merge sort, but instead of numbers, we have trees
;; Basic idea: if any of the heap is empty, just return the other heap
;; else, we extract the first tree of both heaps->t1,t2 and compare the rank of them. If two ranks are different, we would put the tree with smaller rank in front of the result heap,
;; and merge the rest of heap1(t1's heap) with heap2 with t2 unremoved, if the next tree in heap1 is still smaller than t2, same process repeat. 
;; else go to condition3, where two trees have identical rank. 
;; Because of the nature of binomial heap, two trees with same rank would not co-exist inside a heap.
;; We use the insTree helper method and insert the merged t1 t2 using mergeTree method into the merge of the rest of the trees of two heaps assuming they work correctly. As the next tree
;; in two heaps must both be greater than t1, t2 for at least one rank due to binomial heap property, the pre-condition of insTree would be retained. Stacked up insTree and cons would rebuild
;; the result heap we desired.
(define (merge h1 h2)
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else (let ((t1 (car h1)) (t2 (car h2)))
                (cond ((< (rank t1) (rank t2)) (cons t1 (merge(cdr h1) (cons t2 (cdr h2)))))
                      ((< (rank t2) (rank t1)) (cons t2 (merge((cons t1 (cdr h1)) (cdr h2)))))
                      (else (insTree(mergeTree t1 t2) (merge (cdr h1) (cdr h2)))))))))

;; Pre: a heap is inputed
;; Post: the least elt of the heap is returned
;; Basic idea: if the heap is empty, return error by calling (car '()), else if there is just one tree in the heap, return the root node of that tree as
;; using the property of heap, we know that the root node of each tree must be the smallest. Else we simply go through all each tree and compare their root nodes
;; and returns the least value
(define (findmin heap)
  (cond ((null? heap) (car'()))
        ((null? (cdr heap)) (root (car heap)))
        (else (let ((x (root (car heap))) (y (findmin (cdr heap))))
                (if (< x y) x y)))))


;; Pre: a heap is inputed
;; Post: the heap with minimum elt removed would be returned (just like pop in a priority queue)
;; Basic idea: If heap is empty, raise error
;; Else, min is set initally the first tree, we go through the tree and compare the root node of each tree to the min, if they are less than or equal to the min tree,
;; we update the min. (Note that we use <= so that min would be set as the highest tree with highest rank if there is competing minimum, such that the tree with highest
;; rank should be inserted first based on the binomial heap structure), so priority queue property preserve.
;; Then we remove the min tree from the heap, store that in the tmp, store all the children of the mintree in to minchild as they lost the parent.
;; Last, as children of a tree are ranked from highest rank to lower, we reverse that and merge with the tmp to prevent violating the precondition of insTree.

(define (deleteMin heap)
  (define (remove tree h)
    (cond ((null? h) '())
            ((eq? tree (car h)) (cdr h))
            (else (cons (car h) (remove tree (cdr h))))))
  (define (getMin heap min)
    (cond ((null? heap) min)
          ((<= (root (car heap)) (root min)) (getMin (cdr heap) (car heap)))
          (else (getMin (cdr heap) min))))
  (cond ((empty? heap) (car '()))
  (else (let* ((mintree (getMin heap (car heap))) (tmp (remove mintree heap)) (minchild (child mintree)))
    (merge (reverse minchild) tmp)))))















;;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Generalized version, all basic functionality are exactly the same, but with self-defined comparator, so x could be not only the priority, but can also store other information

(define (comp-first root1 root2)
  (if (< (car root1) (car root2)) #t #f))
;; Used for finding the largest rank tree when deleteMin
(define (comp-first-equal root1 root2)
  (if (<= (car root1) (car root2)) #t #f))

(define (mergeTree-g n1 n2 comparator)
  (let ((root1 (root n1)) (root2 (root n2)) (ranking (rank n1)) (child1 (child n1)) (child2 (child n2)))
    (cond ((comparator root1 root2) (Tree root1 (+ ranking 1) (cons n2 child1)))
          (else (Tree root2 (+ ranking  1) (cons n1 child2))))))

(define (insTree-g t1 heap comparator)
  (cond ((empty? heap) (list t1))
        (else (let ((t2 (car heap)))
                (if (< (rank t1) (rank t2)) (cons t1 heap) (insTree-g (mergeTree-g t1 t2 comparator) (cdr heap) comparator))))))

(define (insert-g elt heap comparator)
  (insTree-g (Tree elt 0 '()) heap comparator))

(define (merge-g h1 h2 comparator)
  (cond ((empty? h1) h2)
        ((empty? h2) h1)
        (else (let ((t1 (car h1)) (t2 (car h2)))
                (cond ((< (rank t1) (rank t2)) (cons t1 (merge-g(cdr h1) (cons t2 (cdr h2)) comparator)))
                      ((< (rank t2) (rank t1)) (cons t2 (merge-g((cons t1 (cdr h1)) (cdr h2) comparator))))
                      (else (insTree-g(mergeTree-g t1 t2 comparator) (merge-g (cdr h1) (cdr h2) comparator) comparator)))))))

(define (findmin-g heap comparator)
  (cond ((null? heap) (car'()))
        ((null? (cdr heap)) (root (car heap)))
        (else (let ((x (root (car heap))) (y (findmin-g (cdr heap) comparator)))
                (if (comparator x y) x y)))))

(define (deleteMin-g heap comparator)
  (define (remove tree h)
    (cond ((null? h) '())
            ((eq? tree (car h)) (cdr h))
            (else (cons (car h) (remove tree (cdr h))))))
  (define (getMin heap min)
    (cond ((null? heap) min)
          ((comparator (root (car heap)) (root min)) (getMin (cdr heap) (car heap)))
          (else (getMin (cdr heap) min))))
  (cond ((empty? heap) (car '()))
  (else (let* ((mintree (getMin heap (car heap))) (tmp (remove mintree heap)) (minchild (child mintree)))
    (merge-g (reverse minchild) tmp comparator)))))




  
;; Test cases
(define Tree1 (Tree 5 0 '()))
(define Tree2 (Tree 3 0 '()))
(define Tree3 (mergeTree Tree1 Tree2))
(define new-heap (insTree Tree3 (empty-heap)))
;;new-heap
(define heap2 (insert 9 (insert 8 (insert 2 (insTree Tree2 (empty-heap))))))
heap2
(define heap3 (insTree Tree3 new-heap))
heap3
(define heap4 (merge heap2 heap3))
heap4
(findmin (insert 0 heap2))
(define heap5 (merge heap3 heap4))
heap5
(define heap6 (deleteMin heap5))
heap6
(deleteMin heap6)



(display "general version")
(display "\n")
(display "----------------------------------------------------------------------------------------------------")
(display "\n")
(define Tree1 (Tree '(5 ls) 0 '()))
(define Tree2 (Tree '(3 cd) 0 '()))
(define Tree3 (mergeTree-g Tree1 Tree2 comp-first))
(define new-heap (insTree-g Tree3 (empty-heap) comp-first))
(define heap2 (insert-g '(9 del) (insert-g '(8 malloc) (insert-g '(2 pop) (insTree-g Tree2 (empty-heap) comp-first) comp-first) comp-first) comp-first))
heap2
(define heap3 (insTree-g Tree3 new-heap comp-first))
heap3
(define heap4 (merge-g heap2 heap3 comp-first))
heap4
(findmin-g (insert-g '(0 shutdown) heap2 comp-first) comp-first)
(define heap5 (merge-g heap3 heap4 comp-first))
heap5
(define heap6 (deleteMin-g heap5 comp-first-equal))
heap6
(deleteMin-g heap6 comp-first-equal)
(findmin-g heap6 comp-first)
(insert-g '(1 project finish) heap6 comp-first)


    