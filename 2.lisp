(in-package :aoc-18)

(defun box-ids (input)
  "Returns the list of box IDs from file INPUT.

For Advent of Code 2018, day 2."
  (with-open-file (in input)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect line)))

(defun count-equal-characters (id hash &aux (result (list 0 0)))
  "Count the equal characters in ID. " ; Genauer beschreiben!
  (loop :for c :across id
        :for i :from 1
        :for rest = (subseq id i)
        :if (not #1=(gethash c hash)) ; Character was not found yet
        :do (setf #1# (1+ (count c rest))))
  (loop :for c :being :the hash-values :of hash
        :if (= c 2) :do (setf (first result) 1)
        :if (= c 3) :do (setf (second result) 1))
  result)

(defun aoc-18-02a (&optional (input #p"input2.txt")
                  &aux (hash (make-hash-table))
                       (list-of-input (box-ids input))
                       (result (list 0 0)))
  "Solves the second AOC puzzle 2018."
  (loop :for id :in list-of-input
        :for (two three) = (count-equal-characters id hash)
        :do (incf (first result) two)
            (incf (second result) three)
            (clrhash hash))
  (* (first result) (second result)))

(defun differ-in-one-letter-p (string-a string-b &aux (different 0))
  (loop :for a :across string-a
        :for b :across string-b
        :if (not (equal a b)) :do (incf different))
  (= 1 different))

(defun aoc-18-02b (&optional (input #p"input2.txt")
                   &aux (list-of-input (box-ids input)))
  (iter
    (for id in list-of-input)
    (for rest = (remove id list-of-input))
    (awhen (find-if (curry #'differ-in-one-letter-p id) list-of-input)
      (return (coerce
               (iter
                 (for a in-string id)
                 (for b in-string it)
                 (when (char= a b) (collect a))) 'string)))))

(define-test test-02
  (assert-true (differ-in-one-letter-p "fghij" "fguij"))
  (assert-false (differ-in-one-letter-p "fghij" "fgihj")))
 
