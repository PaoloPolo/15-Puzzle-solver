;;;; heap.lisp

(in-package #:15-Puzzle-solver)

(defstruct (heap (:constructor %make-heap (compare-fn &key key)))
  "Structure for storing the type (min/max-heap) and heap itself"
  ;; Function for o
  (order-fn (if (null key)
		compare-fn
		(lambda (x y)
		  (funcall compare-fn (funcall key x) (funcall key y)))))
  (heap-structure (make-array 0 :adjustable t :fill-pointer t))
  (heap-size 0))

(defun make-heap (compare-fn &key key initial-values)
  "Make a new heap data structure and insert the initial-values into it"
  (let ((heap (%make-heap compare-fn :key key)))
    (map nil #'(lambda (value)
		 (insert-heap value heap))
	 initial-values)
    heap))

(declaim (inline get-children))
(defun get-children (parent)
  "Get the children positions of a given parent"
  (declare (type integer parent))
  (values (+ (* parent 2) 1)
	  (+ (* parent 2) 2)))

(declaim (inline get-parent))
(defun get-parent (child)
  "Get the parent position of a given child"
  (declare (type integer child))
  (values (floor (/ (1- child) 2))))

(defun peek-heap (heap)
  "Only return the topmost element of the heap"
  (with-slots (heap-structure) heap
    (aref heap-structure 0)))

(defun empty-heap (heap)
  "Remove all elements from the heap"
  (with-slots (heap-structure) heap
    (setf (fill-pointer heap-structure) 0)))

(declaim (inline get-heap-size))
(defun get-heap-size (heap)
  "Return the heap size of a given heap"
  (with-slots (heap-size) heap
    heap-size))

(declaim (inline order-posn))
(defun order-posn (posn-x posn-y heap)
  "Apply the order function on two given elements of the heap (syntactic sugar)"
  (with-slots (heap-structure order-fn) heap
    (funcall order-fn
	     (aref heap-structure posn-x)
	     (aref heap-structure posn-y))))

(defun percolate-up (heap posn)
  "For a newly inserted element at the element of the array move it up until its in the right place"
  (with-slots (heap-structure) heap
    (cond
      ((and (not (= posn 0))
	    (not (order-posn (get-parent posn) posn heap)))
       (rotatef (aref heap-structure posn)
		(aref heap-structure (get-parent posn)))
       (percolate-up heap (get-parent posn)))
      (t
       posn))))


(defun percolate-down (heap posn)
  "Used for moving the substitute of a removed element down till its in the right place"
  (with-slots (order-fn heap-structure) heap
    (multiple-value-bind (left-child right-child) (get-children posn)
      (flet ((swap-if-better (posn-x posn-y)
	       (when (order-posn posn-x posn-y heap)
		   (progn
		    (rotatef (aref heap-structure posn-x)
			     (aref heap-structure posn-y))
		    (percolate-down heap posn-x)))))
	(let ((length-index (1- (length heap-structure))))
	  (cond
	    ; Right and left child are existing
	    ((<= right-child length-index)
	     (if (order-posn left-child right-child heap)
		 (swap-if-better left-child posn)
		 (swap-if-better right-child posn)))
	    ; Only left child is filled
	    ((<= left-child length-index)
	     (swap-if-better left-child posn))))))))


(defun remove-heap (heap)
  "Remove the topmost element of the heap and return it"
  (with-slots (heap-structure heap-size) heap
    (if (plusp heap-size)
	(let ((root (aref heap-structure 0)))
	  (setf (aref heap-structure 0) (vector-pop heap-structure))
	  (percolate-down heap 0)
	  (decf heap-size)
	  root)
	nil)))

(defun insert-heap (value heap)
  "Insert a given value into a heap"
  (with-slots (heap-structure heap-size) heap
    (percolate-up heap (vector-push-extend value heap-structure))
    (incf heap-size)))
