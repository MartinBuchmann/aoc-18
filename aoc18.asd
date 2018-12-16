;;;; project-euler.asd
(asdf:defsystem #:aoc18
  :description "My solutions to some Advent of Code puzzles."
  :author "Martin Buchmann"
  :license "Public Domain"
  :serial t
  :depends-on (#:lisp-unit #:cl-annot #:iterate #:anaphora #:alexandria #:cl-ppcre)
  :components ((:file "package")
               (:file "aux-fns")
               (:file "1")
               (:file "2")
               (:file "3")))

