;(in-package #:15-Puzzle-solver)

(defparameter *test-state* #(15 14 1 6 9 11 4 12 0 10 7 3 13 8 5 2))
(defparameter *goal-state* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

(defstruct heap-node
  "Struct representing a single node in the binary heap (used for sorting)"
  (state #()
   :type vector)
  (total-cost 0
   :type fixnum))

(defstruct hash-node
  "Struct representing a single node in a hash-map"
  (parent-state #()
   :type vector)
  (path-cost 0
   :type fixnum))

(declaim (inline member-sublist))
(defun member-sublist (item list)
  "Test if an item is part of a sublist of list"
  (dolist (sublist list)
    (when (member item sublist)
      (return 'T))))

(defun cycles (state &key (goal-state *goal-state*))
  "Get the disjoint cycles of one permutation"
  (let ((cycle-list))
    (loop for item across state
	  do (when (not (member-sublist item cycle-list))
	       (let ((current item)
		     (cycle))
		 (loop while (not (member current cycle))
		       do (push current cycle)
			  (setf current (elt goal-state (position current state))))
		 (push cycle cycle-list))))
    cycle-list))

(defun num-transpositions (state goal-state)
  "Get the number of transpositions needed to represent the disjoint cycles of the permutation"
    (loop for cycle in (cycles state :goal-state goal-state)
	  if (> (length cycle) 1)
	    sum (1- (length cycle)) into number
	  finally (return number)))

(declaim (inline get-position))
(defun get-position (item state side-length)
  "Returns the x and y position of one item in the given state"
  (let ((posn (position item state)))
    `(:x ,(1+ (mod posn side-length))
      :y ,(1+ (floor posn side-length)))))

(declaim (inline manh-distance))
(defun manh-distance (item state goal-state side-length)
  "Returns the manhattan distance for one item"
  (let ((posn-current (get-position item state side-length))
	(posn-goal (get-position item goal-state side-length)))
    (+ (abs (- (getf posn-current :x) (getf posn-goal :x)))
       (abs (- (getf posn-current :y) (getf posn-goal :y))))))

(defun sum-manh-distance (state goal-state side-length)
  "Returns the summed manhattan distance of the puzzle"
  (loop for item across state
	sum (manh-distance item state goal-state side-length) into total
	finally (return total)))

(defun solvable-p (state goal-state side-length)
  "Returns if the puzzle is solvable"
  (evenp (+ (manh-distance 0 state goal-state side-length)
	    (num-transpositions state goal-state))))

	     
(declaim (inline allowed-moves))
(defun allowed-moves (state side-length)
  "Return a list of allowed moves"
  (let ((elements)
	(posn (get-position 0 state side-length)))
    (unless (= (getf posn :x) 1)
      (push 'Left elements))
    (unless (= (getf posn :x) side-length)
      (push 'Right elements))
    (unless (= (getf posn :y) 1)
      (push 'Up elements))
    (unless (= (getf posn :y) side-length)
      (push 'Down elements))
    elements))

(declaim (inline get-next-state))
(defun get-next-state (movement state side-length)
  "Get the next state for any given move and state"
  (let ((posn (position 0 state))
	(next-state (copy-seq state)))
    (cond
      ((equalp movement 'Up)
       (rotatef (aref next-state posn)
		(aref next-state (- posn side-length))))
      ((equalp movement 'Right)
       (rotatef (aref next-state posn)
		(aref next-state (1+ posn))))
      ((equalp movement 'Down)
       (rotatef (aref next-state posn)
		(aref next-state (+ posn side-length))))
      ((equalp movement 'Left)
       (rotatef (aref next-state posn)
		(aref next-state (1- posn)))))
    next-state))

(declaim (inline get-successors))
(defun get-successors (state side-length)
  "Get all successors of one node"
  (mapcar #'(lambda (move)
	      (get-next-state move state side-length))
 	  (allowed-moves state side-length)))

(defun A-Star (start-state goal-state side-length heuristic)
  "Perform an A-Star search for the given start to the given end-node with the given heuristic function"
  (let ((open-list-hash (make-hash-table :test #'equalp :size 100000 :rehash-size 2.0))
	(open-list (make-heap #'< :key #'heap-node-total-cost))
	(closed-list-hash (make-hash-table :test #'equalp :size 100000 :rehash-size 2.0)))
    (when (solvable-p start-state goal-state side-length)
      (flet ((successor-fn (current-state successor-state successor-g)
	       (setf (gethash successor-state open-list-hash)
		     (make-hash-node :parent-state current-state
				     :path-cost successor-g))
	       (insert-heap (make-heap-node :state
					    successor-state
					    :total-cost
					    (+ successor-g
					       (funcall heuristic successor-state goal-state side-length)))
			    open-list)))
	(insert-heap
	 (make-heap-node :state start-state 
			 :total-cost 0)
	 open-list)
	(setf (gethash start-state open-list-hash) (make-hash-node :parent-state #()
								   :path-cost 0))
	(loop for current-node = (remove-heap open-list)
 	      for current-state = (heap-node-state current-node)
	      until (or (= (hash-table-count open-list-hash) 0)
			nil)
	      do
		 (when (equalp current-state goal-state)
		   (return closed-list-hash))
		 (unless (gethash current-state closed-list-hash)
		   (let ((successor-g (1+ (hash-node-path-cost (gethash current-state open-list-hash)))))
		     (setf (gethash current-state closed-list-hash) (gethash current-state open-list-hash))
		     (remhash current-state open-list-hash)
		     (dolist (successor-state (get-successors current-state side-length))
		       (cond
			 ((gethash successor-state closed-list-hash))
			 ((gethash successor-state open-list-hash)
			  (let ((succ-hash-node (gethash successor-state open-list-hash)))
			    (when (< successor-g (hash-node-path-cost succ-hash-node))
			      (successor-fn current-state successor-state successor-g))))
			 (t
			  (successor-fn current-state successor-state successor-g)))
		       (when (equalp successor-state goal-state)
			 (setf (gethash successor-state closed-list-hash)
			       (make-hash-node :parent-state current-state
					       :path-cost successor-g)))))))))))

