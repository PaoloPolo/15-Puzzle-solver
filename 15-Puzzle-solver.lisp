;;;; 15-Puzzle-solver.lisp

(in-package #:15-Puzzle-solver)

(defparameter *initial-state* '(8 4 9 7 3 10 14 15 6 12 1 2 5 11 13 0))

(defparameter *test-state* '(1 2 3 4 5 6 0 7 8))

(defparameter *goal-state* '(1 2 3 4 5 6 7 8 0))

(defparameter *side-length* 3)

(defstruct state-position
  "Structure for storing the position of the empty field"
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun get-position (state)
  "Get the position of the empty field in the given state"
  (setq posn (make-state-position
	      :x (mod (position 0 state) *side-length*)
	      :y (floor (position 0 state) *side-length*)
	      )))

(defun allowed-moves (state)
  "Return a list of allowed moves"
  (let ((elements)
	(posn (get-position state)))
    (when (not (= (state-position-x posn) 0)) (push 'Left elements))
    (when (not (= (state-position-x posn) (1- *side-length*))) (push 'Right elements))
    (when (not (= (state-position-y posn) 0)) (push 'Up elements))
    (when (not (= (state-position-y posn) (1- *side-length*))) (push 'Down elements))
    elements))

(defun member-sublist (item list)
  "Check if the given item is an item of a sublist of the given list"
  (loop for sublist in list
	if (member item sublist)
	  return T
	finally (return Nil)))

(defun cycles (state)
  "Break the permutation up into disjoint cycles"
  (let ((cycles))
    (loop for item in state
	  do (when (not (member-sublist item cycles))
	       (let ((current-item item) (cycle))
		 (loop while (not (member current-item cycle))
		       do (push current-item cycle)
			  (setq current-item (nth (position current-item state) *goal-state*)))
		 (push cycle cycles))))
    cycles))

(defun number-transp (state)
  "Get the number of transpositions needed to represent the permutation to the goal state"
  (loop for cycle in (cycles state)
	if (> (length cycle) 1)
	  sum (1- (length cycle)) into num-transp
	finally (return num-transp)))

(defun solvable-p (state)
  "Check if the state is solvable, if not, the program does not have to proceed"
  (evenp (number-transp state)))


(defun get-next-state (movement state)
  (let* ((posn (position 0 state))
	 (next-state (copy-seq state)))
    (cond
      ((equalp movement 'Up) (rotatef (nth posn next-state) (nth (- posn *side-length*) next-state)))
      ((equalp movement 'Right) (rotatef (nth posn next-state) (nth (1+ posn) next-state)))
      ((equalp movement 'Down) (rotatef (nth posn next-state) (nth (+ posn *side-length*) next-state)))
      ((equalp movement 'Left) (rotatef (nth posn next-state) (nth (1- posn) next-state))))
    next-state))

(defun revert-movement (movement state)
  (cond
    ((equalp movement 'Up) (get-next-state 'Down state))
    ((equalp movement 'Right) (get-next-state 'Left state))
    ((equalp movement 'Down) (get-next-state 'Up state))
    ((equalp movement 'Left) (get-next-state 'Right state))))

(defstruct node
  (state '())
  (path-cost 0 :type fixnum)
  (heur-cost 0 :type fixnum)
  (parent '()))

(defparameter *queue* (make-array 1 :adjustable t :fill-pointer 0)
  "The priority-queue of the states to try next")

(defun total-cost (node)
  "Get the total cost of a node"
  (+ (node-path-cost node) (node-heur-cost node)))

(defun sort-queue (node-1 node-2)
  "Sort the queue by the total cost of the nodes"
  (cond ((> (total-cost node-1) (total-cost node-2)) 'T)
	(T 'Nil)))

(defun add-nodes-queue (&rest nodes)
  (loop for node in nodes
	do (when (not (find node *queue* :test #'equalp))
	     (vector-push-extend node *queue*)))
  (sort *queue* #'sort-queue))

(defun return-path (start node)
  (let ((path '()))
    (loop until (equalp start node)
	  do (push (node-parent node) path)
	     (setf node (find (node-parent node) *queue* :test #'equalp)))
    path))

(defun get-next-nodes (node)
  (let ((state (node-state node)))
    (mapcar #'(lambda (move) (let ((next-node (make-node)))
			       (setf (node-state next-node) (get-next-state move state))
			       next-node))
	    (allowed-moves state))))

(defparameter *closed* (make-array 1 :adjustable t :fill-pointer 0))

(defun neighbor-function (node heuristic)
  (mapcar #'(lambda (next-node) (let ((cost (1+ (node-path-cost node))))
				  (when (and
					 (find next-node *queue* :test #'equalp)
					 (< cost (node-path-cost next-node)))
				    (setf *queue* (remove next-node *queue*)))
				  (when (and
					 (find next-node *closed* :test #'equalp)
					 (< cost (node-path-cost next-node)))
				    (setf *closed* (remove next-node *closed*)))
				  (when (not (or
					      (find next-node *queue*)
					      (find next-node *closed*)))
				    (setf (node-path-cost next-node) cost)
				    (setf (node-parent next-node)
					  (make-node :state (node-state node) :heur-cost (node-heur-cost node) :path-cost (node-path-cost node)))
				    (setf (node-heur-cost next-node) (funcall heuristic (node-state next-node)))
				    (add-nodes-queue next-node)
				    (print next-node))))
	  (get-next-nodes node)))

(defun heuristic (state)
  (loop for item in state
	when (not (= item (nth (position item state) *goal-state*)))
	  sum 1 into heur
	finally (return heur)))

(defun get-inv (state)
  (let ((cycle-list (cycles state))
	(number-inv 0))
    (loop for cycle in cycle-list
	  do (when (>= (length cycle) 2)
	       (loop for i from 0 to (- (length cycle) 2)
		     do	(when (< (nth i cycle) (nth (1+ i) cycle))
			  (setf number-inv (+ (1- (* 2 (- (nth (1+ i) cycle) (nth i cycle)))) number-inv)))))
	  finally (return number-inv))))

(defun A-Star (start goal heuristic)
  (add-nodes-queue start)
  (loop	until (= (length *queue*) 0)
	do (let ((current-node (vector-pop *queue*)))
	     (cond
	       ((not (equalp (node-state current-node) (node-state goal)))
		(vector-push-extend current-node *closed*)
		(neighbor-function current-node heuristic))
	       (t (return Nil))))))
