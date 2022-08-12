;;;; 15-Puzzle-solver.lisp

(in-package #:15-Puzzle-solver)

(defparameter *initial-state* '(8 4 9 7 3 10 14 15 6 12 1 2 5 11 13 0))

(defparameter *goal-state* '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

(defparameter *moves* '(Up Right Down Left))

(defparameter *side-length* 4)

(defstruct state-position
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun get-position (state)
  (setq posn (make-state-position
	      :x (floor (position 0 state) *side-length*)
	      :y (mod (position 0 state) *side-length*)
	      )))


(defun allowed-moves (posn)
  (let ((elements))
    (when (not (= (state-position-x posn) 0)) (push 'Left elements))
    (when (not (= (state-position-x posn) 3)) (push 'Right elements))
    (when (not (= (state-position-y posn) 0)) (push 'Up elements))
    (when (not (= (state-position-y posn) 3)) (push 'Down elements))
    elements))


