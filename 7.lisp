(in-package :aoc-18)
(annot:enable-annot-syntax)

(defun read-input7 (&optional (input #p"inputs/input7.txt"))
  "Reads the input for AOC-18 day 7."
  (iter
    (for line in-file input using #'read-line)
    (collect (register-groups-bind (a b)
                 ("Step (\\w) must be finished before step (\\w) can begin." line)
               (list a b)))))

;;; TODO: Do a topological sort on the tree

