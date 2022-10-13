(defparameter *test-state* '(1 2 3 0 8 7 6 5 4))
(defparameter *goal-state* '(1 2 3 4 5 6 7 8 0))

(defclass node ()
  ((state
    :initarg :state
    :accessor state
    :initform '())
   (path-cost
    :initarg :path-cost
    :accessor path-cost
    :initform 0)
   (total-cost
    :initarg :total-cost
    :accessor total-cost
    :initform 0)
   (parent
    :initarg :parent
    :accessor parent
    :initform 'Nil))
  (:documentation "Class thats representing a single node"))

(defun member-sublist (item list)
  "Test if an item is part of a sublist of list"
  (dolist (sublist list)
    (when (member item sublist)
      (return 'T))))

(defun cycles (state &optional (goal-state *goal-state*))
  "Get the disjoint cycles of one permutation"
  (let ((cycle-list))
    (loop for item in state
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
    (loop for cycle in (cycles state goal-state)
	  if (> (length cycle) 1)
	    sum (1- (length cycle)) into number
	  finally (return number)))

(defun get-position (item state side-length)
  "Returns the x and y position of one item in the given state"
  (let ((posn (position item state)))
    `(:x ,(1+ (mod posn side-length))
      :y ,(1+ (floor posn side-length)))))

(defun manh-distance (item state goal-state side-length)
  "Returns the manhattan distance for one item"
  (let ((posn-current (get-position item state side-length))
	(posn-goal (get-position item goal-state side-length)))
    (+ (abs (- (getf posn-current :x) (getf posn-goal :x)))
       (abs (- (getf posn-current :y) (getf posn-goal :y))))))

(defun sum-manh-distance (state goal-state side-length)
  "Returns the summed manhattan distance of the puzzle"
  (loop for item in state
	sum (manh-distance item state goal-state side-length) into total
	finally (return total)))

(defun solvablep (state goal-state side-length)
  "Returns if the puzzle is solvable"
  (evenp (+ (manh-distance 0 state goal-state side-length)
	    (num-transpositions state goal-state))))

(defun successor-fn (node goal-node open-list closed-list heuristic side-length)
  "Perform tasks needed for successors of the given node"
  (mapcar #'(lambda (curr-successor)
	      (let ((curr-path-cost (1+ (path-cost node))))
		(if (or (find curr-successor closed-list :test #'(lambda (node-1 node-2)
								   (equalp (state node-1)
									   (state node-2))))
			(and (find curr-successor open-list :test #'(lambda (node-1 node-2)
								      (equalp (state node-1)
									      (state node-2))))
			     (< (total-cost node) curr-path-cost)))
		    'Nil
		    (setf open-list (add-node-queue open-list curr-successor)))))
	  (get-successors node goal-node side-length heuristic))
  open-list)

(defun A-Star (start-node goal-node side-length heuristic)
  (let ((open-list '())
	(closed-list '())
	(current-node '()))
    (push start-node open-list)
    (loop until (= (length open-list) 0)
	  do (setf current-node (pop open-list))
	     (pprint open-list)
	     (push current-node closed-list)
	     (cond ((equalp (state current-node) (state goal-node)) 'Found)
		   (t (push current-node closed-list)
		      (setf open-list (successor-fn current-node
						    goal-node
						    open-list
						    closed-list
						    heuristic
						    side-length)))))))

(defun allowed-moves (state side-length)
  "Return a list of allowed moves"
  (let ((elements)
	(posn (get-position 0 state side-length)))
    (when (not (= (getf posn :x) 1)) (push 'Left elements))
    (when (not (= (getf posn :x) side-length)) (push 'Right elements))
    (when (not (= (getf posn :y) 1)) (push 'Up elements))
    (when (not (= (getf posn :y) side-length)) (push 'Down elements))
    elements))

(defun get-next-state (movement state side-length)
  (let ((posn (position 0 state))
	(next-state (copy-list state)))
    (cond
      ((equalp movement 'Up)
       (rotatef (nth posn next-state)
		(nth (- posn side-length) next-state)))
      ((equalp movement 'Right)
       (rotatef (nth posn next-state)
		(nth (1+ posn) next-state)))
      ((equalp movement 'Down)
       (rotatef (nth posn next-state)
		(nth (+ posn side-length) next-state)))
      ((equalp movement 'Left)
       (rotatef (nth posn next-state)
		(nth (1- posn) next-state))))
    next-state))

(defun get-successors (node goal-node side-length heuristic)
  (mapcar #'(lambda (move)
	      (let ((curr-path-cost (1+ (path-cost node)))
		    (curr-heur-cost (funcall heuristic (state node) (state goal-node) side-length)))
		(make-instance 'node :state (get-next-state move (state node) side-length)
				     :path-cost curr-path-cost
				     :total-cost (+ curr-path-cost curr-heur-cost)
				     :parent node)))
	  (allowed-moves (state node) side-length)))

(defun sort-queue (node-1 node-2)
  "Sort the by the total cost of the nodes"
  (< (total-cost node-1) (total-cost node-2)))

(defun add-node-queue (list node)
  (unless (find node list :test #'(lambda (node-1 node-2)
				    (equalp (state node-1)
					    (state node-2))))
    (setq list (merge 'list (list node) list #'sort-queue)))))))
