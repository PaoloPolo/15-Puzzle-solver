;;;; 8-Puzzle-solver.asd

(asdf:defsystem #:8-Puzzle-solver
  :description "A 8/15-Puzzle-solver written in Common Lisp"
  :author "Justus Mehl <justus.mehl@gmx.de>"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :components ((:file "package")
	       (:file "heap")
               (:file "8-Puzzle-solver")
	       (:file "test"))
  :build-operation "program-op"
  :build-pathname "8-Puzzle-solver"
  :entry-point "8-Puzzle-solver:solve-puzzle")
