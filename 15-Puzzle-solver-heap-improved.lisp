(in-package #:15-Puzzle-solver)

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
  (and (every #'identity
	      (mapcar #'(lambda (curr-state)
			  (and (= (length curr-state) (expt side-length 2))
			       (every #'(lambda (item)
					  (= 1 (count item curr-state)))
				      curr-state)
			       (= 1 (count free-space curr-state))))
		      (list state goal-state)))
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

(defun build-moves (free-space side-length path)
  "Returns the needed moves for reaching the goal-state"
  (let ((moves '()))
    (dotimes (i (1- (length path)))
      (let ((posn-state (position free-space (nth i path)))
	    (posn-next (position free-space (nth (1+ i) path))))
	(cond
	  ((= posn-next (- posn-state side-length))
	   (push 'Up moves))
	  ((= posn-next (1+ posn-state))
	   (push 'Right moves))
	  ((= posn-next (+ posn-state side-length))
	   (push 'Down moves))
	  ((= posn-next (1- posn-state))
	   (push 'Left moves)))))
    (reverse moves)))

(defun A-Star (free-space start-state goal-state side-length heuristic &key (node-maximum nil node-maximum-p))
  "Perform an A-Star search for the given start to the given end-node with the given heuristic function"
  (let ((open-list (make-heap #'< :key #'node-total-cost))
	(closed-list (make-hash-table :test #'equalp :size 100000 :rehash-size 1.5)))
    (when (solvable-p free-space start-state goal-state side-length)
      (insert-heap (make-node :state start-state
			      :total-cost (funcall heuristic start-state goal-state side-length))
		   open-list)
      (loop until (or (= (get-heap-size open-list) 0)
		      (equalp current-state goal-state)
		      (and node-maximum-p
			   (> expanded-nodes node-maximum)))
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
		      (return (list (build-path start-state
						goal-state
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

(declaim (inline print-state))
(defun print-state (state side-length &key (stream t))
  (flet ((print-line ()
	   (dotimes (i (1+ (* 3 side-length)))
	     (format stream "-"))
	   (format stream "~%"))
	 (print-items (row)
	   (format stream "|")
	   (dotimes (i side-length)
	     (format stream "~vd|" 2 (aref state (+ (* row side-length) i))))
	   (format stream "~%")))
    (print-line)
    (dotimes (i side-length)
      (print-items i)
      (print-line))))

(defun read-state (side-length name)
  "Read in a state with the given side-length"
  (format t "Now the ~a state has to be entered~%" name)
  (format t "The single rows are read in, please enter the numbers divided by one space~%")
  (force-output)
  (let ((temp '()))
    (dotimes (i side-length)
      (format t "Please enter the line ~D of your puzzle: " (1+ i))
      (force-output)
      (setq temp (append temp (with-input-from-string (in (read-line))
				(loop for x = (read in nil nil) while x collect x)))))
    (coerce temp 'vector)))

(defun solve-puzzle ()
  "Solve the puzzle by using the A-Star algorithm and print the solution if one was found"
  (flet ((empty-line () (format t "~%")))
    (let ((side-length 0)
	  (start-state)
	  (goal-state)
	  (heuristic)
	  (free-space 0))
      (loop do
	(format t "Please enter the side-length of your puzzle: ")
	(force-output)
	(loop for input = (read)
	      until (and (numberp input)
			 (> input 1))
	      finally (setf side-length input))
	(empty-line)
	(format t "Please enter the representation of the free space: ")
	(force-output)
	(loop for input = (read)
	      until (numberp input)
	      finally (setf free-space input))
	(empty-line)
	(setf start-state (read-state side-length "start"))
	(empty-line)
	(setf goal-state (read-state side-length "goal"))
	(empty-line)
	(format t "Please enter whether you want to use the Manhattan-Distance (MD) or Misplaced Tiles heuristic (MT): ")
	(force-output)
	(loop for input = (read)
	      until (or (string-equal input "MD")
			(string-equal input "MT"))
	      finally (cond ((string-equal input "MD")
			     (setf heuristic #'sum-manh-distance))
			    ((string-equal input "MT")
			     (setf heuristic #'misplaced-tiles))))
	(empty-line)
	(if (solvable-p free-space start-state goal-state side-length)
	    (multiple-value-bind (return-value time-ratio)
		(with-timing (A-Star free-space start-state goal-state side-length heuristic))
	      (let* ((path (first return-value))
		     (solution (build-moves free-space side-length path))
		     (expanded-nodes (second return-value)))
		(if (not (null return-value))
		    (progn (format t "Solution was found with ~r step~:p in ~F second~:p~%"
				   (length solution) time-ratio)
			   (format t "For that ~D node~:p had to be expanded.~%" expanded-nodes)
			   (format t "The step~p needed: ~%~%~{~a~^, ~}~%"
				   (length solution)
				   (mapcar #'(lambda (x)
					       (cond((equalp x 'Up)
						     "Up")
						    ((equalp x 'Right)
						     "Right")
						    ((equalp x 'Down)
						     "Down")
						    ((equalp x 'Left)
						     "Left")))
					   solution))
			   (empty-line)
			   (when (y-or-n-p
				  "Should the steps be pretty printed?")
			     (empty-line)
			     (dolist (state path)
			       (print-state state side-length)
			       (empty-line))))
		    (format t "The puzzle could not be solved. Elapsed time is ~F second~:p~%"
			    time-ratio))
		(not (null return-value))))
	    (progn (format t "The puzzle is not solvable")
		   (force-output)))
	    until (not (y-or-n-p "Do you want to repeat the search?~%"))))))
