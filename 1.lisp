(in-package :aoc-18)

;;; We use iterate now
;;; The file is clean and contains no garbage etc.
(defun aoc-18-01a ()
  "Solves the first AOC puzzle 2018."
  (iter (for number in-file #p"inputs/input-01.txt")
    (sum number)))

(defun get-frequency-list (input)
  "Returns the list of frequency from file INPUT.

For Advent of Code 2018, day 1."
  (iter (for number in-file input)
    (collect number)))

(defun aoc-18-01b (&optional (input #p"inputs/input-01.txt")
                   &aux (hash (make-hash-table))
                        (list-of-input (get-frequency-list input))
                        (length-of-input (length list-of-input)))
  "Solves the second puzzle of day 1 AOC 2018."
  (iter
    (for i first 0 then (if (< i (1- length-of-input)) (1+ i) (- i (1- length-of-input))))
    (for frequency-change = (nth i list-of-input))
    (for frequency first frequency-change then (+ frequency frequency-change))
    (if #1=(gethash frequency hash)
        frequency
        (setf #1# 1))))
