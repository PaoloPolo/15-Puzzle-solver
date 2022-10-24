(in-package :15-Puzzle-solver)

(defun tests ()
  "Do some tests and return the values in the result.txt"
  (with-open-file (stream "results.txt" :direction :output)
    (flet ((call-a-star
	       (&key
		  (free-space 0)
		  test-state
		  (goal-state #(1 2 3 4 5 6 7 8 0))
		  (side-length 3))
	     (format stream "Start:~%")
	     (print-state test-state side-length :stream stream)
	     (format stream "End:~%")
	     (print-state goal-state side-length :stream stream)
	     (format stream "~%~%")
	     (mapcar #'(lambda (heuristic)
			 (if (solvable-p free-space test-state goal-state side-length)
			     (progn
			       (cond ((equalp heuristic #'sum-manh-distance)
				      (format stream "Sum Manhattan-Distance:~%"))
				     ((equalp heuristic #'misplaced-tiles)
				      (format stream "Misplaced-Tiles:~%")))
			       (multiple-value-bind (return-value time-ratio)
				   (with-timing (A-star free-space
							test-state
							goal-state
							side-length
							heuristic
							:node-maximum 3000000))
				 (let* ((path (first return-value))
					(solution (build-moves free-space side-length path))
					(expanded-nodes (second return-value)))
				   (if (not (null return-value))
				       (progn (format stream "Solution was found with ~r step~:p in ~F second~:p~%"
						      (length solution) time-ratio)
					      (format stream "For that ~D node~:p had to be expanded.~%"
						      expanded-nodes)
					      (format stream "The step~p needed: ~%~%~{~a~^, ~}~%~%"
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
							      solution)))
				       (format stream
					       "The puzzle could not be solved. Elapsed time is ~F second~:p~%~%"
					       time-ratio)))))
			     (format t "The puzzle is not solvable")))
		     (list #'sum-manh-distance #'misplaced-tiles))
	     (format stream "~%~%")))
      (call-a-star :test-state #(1 2 3 4 5 6 0 7 8))
      (call-a-star :test-state #(0 1 2 5 6 3 4 7 8))
      (call-a-star :test-state #(1 2 3 5 7 0 4 8 6))
      (call-a-star :test-state #(2 5 7 8 4 0 1 3 6))
      (call-a-star :test-state #(8 6 7 2 5 4 3 0 1))
      (call-a-star :test-state #(5 0 2 7 6 13 1 4 9 11 10 3 14 15 12 8)
		   :goal-state #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0)
		   :side-length 4))))
