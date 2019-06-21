;;;; project-euler.asd
(asdf:defsystem #:aoc18
  :description "My solutions to some Advent of Code 2018 puzzles."
  :author "Martin Buchmann"
  :license "Public Domain"
  :serial t
  :depends-on (#:lisp-unit #:cl-annot #:iterate #:anaphora #:alexandria #:cl-ppcre #:cl-arrows
               #:cl-digraph #:cl-digraph.dot #:cl-containers #:cl-cairo2)
  :components ((:file "package")
               (:file "aux-fns")
               (:file "clist")
               (:file "1")
               (:file "2")
               (:file "3")
               (:file "4")
               (:file "5")
               (:file "6")
               (:file "7")
               (:file "8")
               (:file "9")
               (:file "10")
               (:file "11")
               (:file "12")
               (:file "13")
               (:file "14")
               (:file "15")))
