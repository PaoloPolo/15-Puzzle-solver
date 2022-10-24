;;;; 15-Puzzle-solver.asd

(asdf:defsystem #:15-Puzzle-solver
  :description "A 8/15-Puzzle-solver written in common lisp"
  :author "Justus Mehl <justus.mehl@gmx.de>"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :components ((:file "package")
	       (:file "heap")
               (:file "15-Puzzle-solver-heap-improved")
	       (:file "test"))
  :build-operation "program-op"
  :build-pathname "15-Puzzle-solver1"
  :entry-point "15-Puzzle-solver:solve-puzzle")
