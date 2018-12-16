(in-package :aoc-18)

(iter
  (for line in-file #p"test-3.txt" using #'read-line)
  (for list-of-line = (split #\space line))
  (collect (list (nth 2 list-of-line) (nth 3 list-of-line))))
