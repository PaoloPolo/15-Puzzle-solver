;;;; 15-Puzzle-solver.asd

(asdf:defsystem #:15-Puzzle-solver
  :description "A 8/15-Puzzle-solver written in common lisp"
  :author "Justus Mehl <justus.mehl@gmx.dem>"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "15-Puzzle-solver")))
