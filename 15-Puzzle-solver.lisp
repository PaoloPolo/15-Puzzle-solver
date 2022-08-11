;;;; 15-Puzzle-solver.lisp

(in-package #:15-Puzzle-solver)

(defparameter *initial-state* '(8 4 9 7 3 10 14 15 6 12 1 2 5 11 13 0))

(defparameter *goal-state* '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0))

(defun allowed-moves (state)
  (let (index (position 0 state))
    (cond ((= index NIL) (NIL))
	  ((= index 0) ('(R D)))
	  ((= index 1) or (= index 2) ('(R D L)))
	  ((= index 4) ('(D L)))
	  ((= index ))))


  )
