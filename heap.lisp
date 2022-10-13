;;;; heap.lisp

(in-package #:15-Puzzle-solver)

(defstruct (heap (:constructor %make-heap (compare-fn &key key)))
  "Structure for storing the type (min/max-heap) and heap itself"
  ;; Function for o
  (order-fn (if (null key)
		compare-fn
	      (lambda (x y)
		  (funcall compare-fn (funcall key x) (funcall key y)))))
  (heap-structure (make-array 0 :adjustable t :fill-pointer t)))

(defun make-heap (compare-fn &key key initial-values)
  "Make a new heap data structure and insert the initial-values into it"
  (let ((heap (%make-heap compare-fn :key key)))
    (map nil #'(lambda (value)
		 (insert-heap value heap))
	 initial-values)
    heap))

(declaim (inline get-children))
(defun get-children (parent)
  (declare (type integer parent))
  (values (+ (* parent 2) 1)
	  (+ (* parent 2) 2)))

(declaim (inline get-parent))
(defun get-parent (child)
  (declare (type integer child))
  (values (floor (/ (1- child) 2))))

(defun insert-heap (value heap)
  "Insert a given value into a heap"
  (with-slots (order-fn heap-structure) heap
    (percolate-up heap (vector-push-extend value heap-structure))))

(defun remove-heap (heap)
  "Remove the topmost element of the heap and return it"
  (with-slots (order-fn heap-structure) heap
    (if (plusp (length heap-structure))
	(let ((root (aref heap-structure 0)))
	  (setf (aref heap-structure 0) (vector-pop heap-structure))
	  (percolate-down heap 0)
	  root)
	nil)))

(defun peek-heap (heap)
  "Only return the topmost element of the heap"
  (with-slots (heap-structure) heap
    (aref heap-structure 0)))

(defun empty-heap (heap)
  "Remove all elements from the heap"
  (with-slots (heap-structure) heap
    (setf (fill-pointer heap-structure) 0)))

(defun percolate-up (heap posn)
  (with-slots (order-fn heap-structure) heap
    (cond
      ((and (/= posn 0)
	    (not (funcall order-fn
			  (aref heap-structure (get-parent posn))
			  (aref heap-structure posn)
			  )))
       (rotatef (aref heap-structure posn)
		(aref heap-structure (get-parent posn)))
       (percolate-up heap (get-parent posn))
       )
      (t
       posn))))


(defun percolate-down (heap posn)
  (with-slots (order-fn heap-structure) heap
    (multiple-value-bind (left-child right-child) (get-children posn)
      (when (and (< (get-children posn)
		    (length heap-structure))
		 (not
		  (and (funcall order-fn
				(aref heap-structure posn)
				(aref heap-structure right-child)
				)
		       (or
			(>= (1+ (get-children posn)) (length heap-structure))
			(funcall order-fn
				 (aref heap-structure posn)
				 (aref heap-structure right-child))))))
	(let ((chosen-child (if (funcall order-fn
					 (aref heap-structure left-child)
					 (aref heap-structure right-child))
				left-child
				right-child)))
	  (rotatef (aref heap-structure posn)
		   (aref heap-structure chosen-child))
	  (percolate-down heap chosen-child))))))
