;(in-package #:15-Puzzle-solver)

(defparameter *test-state* #(8 6 7 2 5 4 3 0 1))
(defparameter *goal-state* #(1 2 3 4 5 6 7 8 0))

(defstruct node
  "Struct representing a single node in the binary heap (used for sorting)"
  (state #()
   :type simple-vector)
  (parent-state #()
   :type simple-vector)
  (path-cost 0
   :type fixnum)
  (total-cost 0
   :type fixnum))

(declaim (inline member-sublist))
(defun member-sublist (item list)
  "Test if an item is part of a sublist of list"
  (dolist (sublist list)
    (when (member item sublist)
      (return 'T))))

(defun cycles (state goal-state)
  "Get the disjoint cycles of one permutation"
  (let ((cycle-list))
    (loop for item across state
	  do (when (not (member-sublist item cycle-list))
	       (let ((current item)
		     (cycle))
		 (loop until (member current cycle)
		       do (push current cycle)
			  (setf current (aref goal-state (position current state))))
		 (push cycle cycle-list))))
    cycle-list))

(defun num-transpositions (state goal-state)
  "Get the number of transpositions needed to represent the disjoint cycles of the permutation"
    (loop for cycle in (cycles state goal-state)
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

(declaim (inline sum-manh-distance))
(defun sum-manh-distance (state goal-state side-length)
  "Returns the summed manhattan distance of the puzzle"
  (loop for item across state
	sum (manh-distance item state goal-state side-length) into total
	finally (return total)))

(declaim (inline misplaced-tiles))
(defun misplaced-tiles (state goal-state side-length)
  "Returns the manhattan distance for one item"
  (declare (ignore side-length))
  (loop for x across state
	for y across goal-state
	when (not (= x y))
	  sum 1 into misplaced 
	finally (return misplaced)))

(defun solvable-p (free-space state goal-state side-length)
  "Returns if every item is present only once and if the puzzle is solvable in general"
  (and (= (length state) (expt side-length 2))
       (= (length goal-state) (expt side-length 2))
       (every #'(lambda (item)
		  (= 1 (count item state)))
	      state)
       (every #'(lambda (item)
		  (= 1 (count item goal-state)))
	      goal-state)
       (evenp (+ (manh-distance free-space state goal-state side-length)
		 (num-transpositions state goal-state)))))
	     
(declaim (inline allowed-moves))
(defun allowed-moves (state free-space side-length)
  "Return a list of allowed moves"
  (let ((elements)
	(posn (get-position free-space state side-length)))
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
(defun get-next-state (movement free-space state side-length)
  "Get the next state for any given move and state"
  (let ((posn (position free-space state))
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
(defun get-successors (node goal-state free-space side-length heuristic)
  "Get all successors of one node"
  (let ((path-cost (1+ (node-path-cost node))))
    (mapcar #'(lambda (move)
		(let ((state (get-next-state move free-space (node-state node) side-length)))
		  (make-node :state state
			     :parent-state (node-state node)
			     :path-cost path-cost
			     :total-cost (max (+ path-cost (funcall heuristic state goal-state side-length))
					      (node-total-cost node)))))
 	    (allowed-moves (node-state node) free-space side-length))))

(defun build-path (start-state goal-state closed-list)
  "Return the path from the start state to the goal state"
  (let ((path '())
	(parent (gethash goal-state closed-list)))
    (push goal-state path)
    (loop
      (push parent path)
      (when (equalp parent start-state)
	(return path))
      (setf parent (gethash parent closed-list)))))

(defun build-moves (start-state goal-state free-space side-length closed-list)
  "Returns the needed moves for reaching the goal-state"
  (let ((path (build-path start-state goal-state closed-list))
	(moves '()))
    (dotimes (i (1- (length path)))
      (let ((posn-state (position free-space (nth i path)))
	    (posn-next (position free-space (nth (1+ i) path))))
	(cond
	  ((= posn-next (- posn-state side-length))
	   (push "Up" moves))
	  ((= posn-next (1+ posn-state))
	   (push "Right" moves))
	  ((= posn-next (+ posn-state side-length))
	   (push "Down" moves))
	  ((= posn-next (1- posn-state))
	   (push "Left" moves)))))
    (reverse moves)))

(defun A-Star (free-space start-state goal-state side-length heuristic)
  "Perform an A-Star search for the given start to the given end-node with the given heuristic function"
  (let ((open-list (make-heap #'< :key #'node-total-cost))
	(closed-list (make-hash-table :test #'equalp :size 100000 :rehash-size 2.0)))
    (when (solvable-p free-space start-state goal-state side-length)
      (insert-heap (make-node :state start-state
			      :total-cost (funcall heuristic start-state goal-state side-length))
		   open-list)
      (loop until (or (= (get-heap-size open-list) 0)
		      (equalp current-state goal-state))
	    for current-node = (remove-heap open-list)
 	    for current-state = (node-state current-node)
	    for expanded-nodes from 1
	    do
	       (unless (gethash current-state closed-list)
		 (setf (gethash current-state closed-list)
		       (node-parent-state current-node))
		 (dolist (successor-node (get-successors current-node goal-state free-space side-length heuristic))
		   (cond
		     ((gethash (node-state successor-node) closed-list))
		     (t
		      (insert-heap successor-node open-list)))
		   (when (equalp (node-state successor-node) goal-state)
		     (setf (gethash (node-state successor-node) closed-list) current-state))))
	    finally (when (equalp current-state goal-state)
		      (remove-heap open-list)
		      (return (list (build-moves start-state
						   goal-state
						   free-space
						   side-length
						   closed-list)
				      expanded-nodes)))))))

(defmacro with-timing (&body function-forms)
  "Return the time spent in the called function (ratio) and the return-value of the function"
  (let ((start-time (gensym))
	(end-time (gensym))
	(return-value (gensym)))
    `(let* ((,start-time (get-internal-real-time))
	    (,return-value (progn ,@function-forms))
	    (,end-time (get-internal-real-time)))
       (values ,return-value (/ (- ,end-time ,start-time) internal-time-units-per-second)))))
 
(defun solve-puzzle (start-state goal-state side-length heuristic &key (free-space 0))
  "Solve the puzzle by using the A-Star algorithm and print the solution if one was found"
  (multiple-value-bind (return-value time-ratio)
      (with-timing (A-Star free-space start-state goal-state side-length heuristic))
    (let ((solution (first return-value))
	  (expanded-nodes (second return-value)))
      (if (not (null return-value))
	  (progn (format t "Solution was found with ~r step~:p in ~F second~:p: ~%~{~a~^, ~}~%"
			 (length solution) time-ratio solution)
		 (format t "For the solution ~D node~:p had to be expanded"
			 expanded-nodes))
	  (format t "No solution was found. Elapsed time is ~F second~:p"
		  time-ratio))
      (not (null return-value)))))
