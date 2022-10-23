;(in-package #:15-Puzzle-solver)

(defparameter *test-state* #(15 14 1 6 9 11 4 12 0 10 7 3 13 8 5 2))
(defparameter *goal-state* #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

(defstruct node
  "Struct representing a single node"
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
(defun get-successors (node goal-state free-space side-length heuristic)
  "Get all successors of one node"
  (let ((path-cost (1+ (node-path-cost node))))
    (mapcar #'(lambda (move)
		(let ((state (get-next-state move free-space (node-state node) side-length)))
		  (make-node :state state
			     :path-cost path-cost
			     :total-cost (max (+ path-cost (funcall heuristic state goal-state side-length))
					      (node-total-cost node)))))
 	    (allowed-moves (node-state node) free-space side-length))))

(defun build-path (start-state goal-node closed-list-hash)
  "Return the path from the start state to the goal state"
  (let ((path '())
	(current-node (gethash (node-state goal-node) closed-list-hash)))
    (push (node-state goal-node) path)
    (loop
      (push (node-parent-state current-node) path)
      (when (equalp (node-parent-state current-node) start-state)
	(return (reverse path)))
      (setf current-node (gethash (node-parent-state current-node) closed-list-hash)))))

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

(defun rbfs-search (limit free-space start-state goal-state side-length heuristic)
  "Recursive best first search"
  (when (solvable-p free-space start-state goal-state side-length)
    (rbfs limit
	  free-space
	  (make-node :state start-state
		     :path-cost 0
		     :total-cost (funcall heuristic start-state goal-state side-length))
	  goal-state
	  side-length
	  heuristic
	  limit)))

(defun rbfs (limit free-space node goal-state side-length heuristic max-size)
  "Perform the recursion for the search"
  (if (equalp (node-state node) goal-state)
      (values node limit)
      (let ((successors (make-heap #'<
				   :key #'node-total-cost
				   :initial-values (get-successors node goal-state free-space side-length heuristic))))
	(if (= 0 (get-heap-size successors))
	    (values 'Failure max-size)
	    (loop
	      (let* ((best (remove-heap successors))
		     (best-cost (node-total-cost best))
		     (alternative (node-total-cost (peek-heap successors))))
		(if (> best-cost limit)
		    (return (values 'Failure best-cost))
		    (multiple-value-bind (result lowest-total-cost)
			(rbfs (min limit alternative)
			      free-space
			      best
			      goal-state
			      side-length
			      heuristic
			      max-size)
		      (setf (node-total-cost best) lowest-total-cost)
		      (insert-heap best successors)
		      (unless (equalp result 'Failure)
			(return (values (cons best result) lowest-total-cost)))))))))))

(defmacro with-timing (&body function-forms)
  "Return the time spent in the called function (ratio) and the return-value of the function"
  (let ((start-time (gensym))
	(end-time (gensym))
	(return-value (gensym)))
    `(let* ((,start-time (get-internal-real-time))
	    (,return-value (progn ,@function-forms))
	    (,end-time (get-internal-real-time)))
       (values ,return-value (/ (- ,end-time ,start-time) internal-time-units-per-second)))))
 
;(defun solve-puzzle (start-state goal-state side-length heuristic &key (free-space 0 ))
;  "Solve the puzzle by using the A-Star algorithm and print the solution if one was found"
;  (multiple-value-bind (return-value time-ratio)
;      (with-timing (rbfs-search free-space start-state goal-state side-length heuristic))
;    (if (not (null return-value))
;	(format t "Solution was found with ~r step~:p in ~F second~:p: ~%~{~a~^, ~}"
;		(length return-value) time-ratio return-value)
;	(format t "No solution was found. Elapsed time is ~F second~:p"
;		time-ratio))
;    (not (null return-value))))
