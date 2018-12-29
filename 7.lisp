(in-package :aoc-18)
(annot:enable-annot-syntax)

(defun read-input7 (&optional (input #p"inputs/input7.txt"))
  "Reads the input for AOC-18 day 7."
  (iter
    (for line in-file input using #'read-line)
    (collect (register-groups-bind (a b)
                 ("Step (\\w) must be finished before step (\\w) can begin." line)
               (list a b)))))

(defun instructions (steps &aux (instructions (make-hash-table :test #'equal)))
  "Returns a hashtable where the keys B are the dependents of the values A."
  (iter
    (for (a b) in steps)
    ;; Adding the steps
    (when (not #1=(gethash a instructions))
      (setf #1# '()))
    ;; Comleting the hash
    (if (nth-value 1 #2=(gethash b instructions))
        (setf #2# (cons a #2#))
        (setf #2# (list a))))
  instructions)

(defun choose-next-available (instructions)
  "Returns the next key from instructions which is available, i.e. which has no
dependencies."
  (iter
    (for (key value) in-hashtable instructions)
    (unless (gethash key instructions)
      (collect key into result))
    ;; The order should not matter in topology but here we will sort the keys
    ;; alphabetically.
    (finally (return (first (sort result #'string<))))))

(defun remove-step (step instructions)
  "Returns the hash INSTRUCTIONS with the last STEP removed."
  (iter
    (for (key value) in-hashtable instructions)
    (setf (gethash key instructions) (remove step value :test #'equal))
    (remhash step instructions))
  instructions)

(defun follow-instructions (steps)
  "Returns a string with the right order of the instructions."
  (iter
    (for instructions initially (instructions steps)
         then (remove-step next instructions))
    (for next = (choose-next-available instructions))
    (dbg :07-fi ";;; ~A" next)
    (while next)
    (collect next into ordered-instructions)
    (finally (return (map 'string (lambda (s) (char s 0 )) ordered-instructions)))))

(defun aoc18-07a (&optional (input #p"inputs/input7.txt"))
  "The first part of day 7."
  (-> input
      (read-input7)
      (follow-instructions)))


