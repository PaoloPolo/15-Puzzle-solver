;(in-package #:15-Puzzle-solver)

(defparameter *test-state* #(1 2 3 4 15 14 13 12 11 10 9 8 7 6 5 0))
(defparameter *goal-state* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

(defstruct heap-node
  "Struct representing a single node in the binary heap (used for sorting)"
  (state #()
   :type simple-vector)
  (total-cost 0
   :type fixnum))

(defstruct hash-node
  "Struct representing a single node in a hash-map"
  (parent #()
   :type simple-vector)
  (path-cost 0
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

(defun sum-manh-distance (state goal-state side-length)
  "Returns the summed manhattan distance of the puzzle"
  (loop for item across state
	sum (manh-distance item state goal-state side-length) into total
	finally (return total)))

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
(defun get-successors (state free-space side-length)
  "Get all successors of one node"
  (mapcar #'(lambda (move)
	      (get-next-state move free-space state side-length))
 	  (allowed-moves state free-space side-length)))

(defun build-path (start-state goal-node closed-list-hash)
  "Return the path from the start state to the goal state"
  (let ((path '())
	(current-node (gethash (heap-node-state goal-node) closed-list-hash)))
    (push (heap-node-state goal-node) path)
    (loop
      (push (hash-node-parent current-node) path)
      (when (equalp (hash-node-parent current-node) start-state)
	(return (reverse path)))
      (setf current-node (gethash (hash-node-parent current-node) closed-list-hash)))))

(defun build-moves (start-state goal-node free-space side-length closed-list-hash)
  "Returns the needed moves for reaching the goal-state"
  (let ((path (build-path start-state goal-node closed-list-hash))
	(moves '()))
    (dotimes (i (1- (length path)))
      (let ((posn-state (position free-space (nth i path)))
	    (posn-next (position free-space (nth (1+ i) path))))
	(cond
	  ((= (+ posn-state side-length) posn-next)
	   (push "Up" moves))
	  ((= (1+ posn-state) posn-next)
	   (push "Right" moves))
	  ((= (- posn-state side-length) posn-next)
	   (push "Down" moves))
	  ((= (1- posn-state) posn-next)
	   (push "Left" moves)))))
    (reverse moves)))

(defun A-Star (free-space start-state goal-state side-length heuristic)
  "Perform an A-Star search for the given start to the given end-node with the given heuristic function"
  (let ((open-list-hash (make-hash-table :test #'equalp :size 100000 :rehash-size 2.0))
	(open-list (make-heap #'< :key #'heap-node-total-cost))
	(closed-list-hash (make-hash-table :test #'equalp :size 100000 :rehash-size 2.0)))
    (when (solvable-p free-space start-state goal-state side-length)
      (flet ((successor-fn (current-state successor-state successor-g)
	       (setf (gethash successor-state open-list-hash)
		     (make-hash-node :parent current-state
				     :path-cost successor-g))
	       (insert-heap (make-heap-node :state
					    successor-state
					    :total-cost
					    (+ successor-g
					       (funcall heuristic successor-state goal-state side-length)))
			    open-list)))
	(loop for current-node = (remove-heap open-list)
 	      for current-state = (heap-node-state current-node)
		initially (progn
			    (insert-heap
			     (make-heap-node :state start-state 
					     :total-cost 0)
			     open-list)
			    (setf (gethash start-state open-list-hash) (make-hash-node :parent #()
										       :path-cost 0)))
	      until (or (= (hash-table-count open-list-hash) 0)
			(equalp current-state goal-state))
	      do
		 (unless (gethash current-state closed-list-hash)
		   (let ((successor-g (1+ (hash-node-path-cost (gethash current-state open-list-hash)))))
		     (setf (gethash current-state closed-list-hash) (gethash current-state open-list-hash))
		     (remhash current-state open-list-hash)
		     (dolist (successor-state (get-successors current-state free-space side-length))
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
			       (make-hash-node :parent current-state
					       :path-cost successor-g))))))
	      finally (when (equalp current-state goal-state)
			(return (build-moves start-state current-node free-space side-length closed-list-hash))))))))

(defmacro with-timing (&body function-forms)
  "Return the time spent in the called function (ratio) and the return-value of the function"
  (let ((start-time (gensym))
	(end-time (gensym))
	(return-value (gensym)))
    `(let* ((,start-time (get-internal-real-time))
	    (,return-value (progn ,@function-forms))
	    (,end-time (get-internal-real-time)))
       (values ,return-value (/ (- ,end-time ,start-time) internal-time-units-per-second)))))
 
(defun solve-puzzle (start-state goal-state side-length heuristic &key (free-space 0 ))
  "Solve the puzzle by using the A-Star algorithm and print the solution if one was found"
  (multiple-value-bind (return-value time-ratio)
      (with-timing (A-Star free-space start-state goal-state side-length heuristic))
    (if (not (null return-value))
	(format t "Solution was found with ~r step~:p in ~F second~:p: ~%~{~a~^, ~}"
		(length return-value) time-ratio return-value)
	(format t "No solution was found. Elapsed time is ~F second~:p"
		time-ratio))
    (not (null return-value))))
