;;;; project-euler.asd
(asdf:defsystem #:aoc18
  :description "My solutions to some Advent of Code puzzles."
  :author "Martin Buchmann"
  :license "Public Domain"
  :serial t
  :depends-on (#:lisp-unit #:cl-annot #:iterate #:anaphora #:alexandria #:cl-ppcre #:cl-arrows #:log4cl
               #:cl-digraph)
  :components ((:file "package")
               (:file "aux-fns")
               (:file "1")
               (:file "2")
               (:file "3")
               (:file "4")
               (:file "5")
               (:file "6")
               (:file "7")))
