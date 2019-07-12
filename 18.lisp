;;; -*- ispell-local-dictionary: "en_GB" -*-
;; * Day 18
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; * A helper function for the 2D-array

(defun flatten-array (array)
  "Returns a 2D array as 1d array, e.g. for easier counting of elements."
  (make-array (array-total-size array)
              :displaced-to array))

;; * Handling the data of the area

(defparameter *char-alist*
  '((#\. . :open)
    (#\| . :trees)
    (#\# . :lumberyard))
  "Encoding the three different states of the area.")

(defun char-to-keyword (char)
  "Returns the corresponding KEYWORD for CHAR."
  (cdr (assoc char *char-alist*)))

(defun keyword-to-char (keyword)
  "Returns the corresponding CHAR for KEYWORD."
  (car (rassoc keyword *char-alist*)))

(defun print-area (area)
  "Prints the AREA using the pre-definied chars."
  (iter
    (for y below (array-dimension area 0))
    (iter (for x below (array-dimension area 1))
          (format t "~A" (keyword-to-char (aref area y x))))
    (terpri)))

;; * Input

(defun parse-input (input)
  "Reads in a list of string and returns an array of keywords."
  (let ((area (make-array (list (length input) (length (first input)))
                          :element-type 'keyword
                          :initial-element :open)))
    (iter
      (for line in input)
      (for y from 0)
      (iter
        (for char in-string line with-index x)
        (setf (aref area y x) (char-to-keyword char))))
    area))

(defun read-18 (&optional (input #p"inputs/input18.txt"))
  "Returns the input of day 17 as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))


(defparameter *test-area*
  (parse-input '(".#.#...|#."
                 ".....#|##|"
                 ".|..|...#."
                 "..|#.....#"
                 "#.#|||#|#|"
                 "...#.||..."
                 ".|....|..."
                 "||...#|.#|"
                 "|.||||..|."
                 "...#.|..|.")))

;; * Part 1

;; ** Collecting the neighbors of the current cell.
(defun neighbors (area x y)
  "Returns a list of neighbors."
  (iter outer
        (for yp from (max (1- y) 0) to (min (1+ y) (1- (array-dimension area 0))))
        (iter (for xp from (max (1- x) 0) to (min (1+ x) (1- (array-dimension area 1))))
              (when (or (/= x xp) (/= y yp))
                (in outer
                    (collecting (aref area yp xp)))))))

;; ** Calculate the next value of a given cell.

(defun new-value (area x y)
  "Returns the next value of the cell X Y as given by its neighbors."
  (let ((current (aref area y x))
        (neighbors (neighbors area x y)))
    (cond
      ((and (eq current :open)
            (<= 3 (count :trees neighbors)))
       :trees)
      ((and (eq current :trees)
            (<= 3 (count :lumberyard neighbors)))
       :lumberyard)
      ((and (eq current :lumberyard)
            (<= 1 (count :lumberyard neighbors))
            (<= 1 (count :trees neighbors)))
       :lumberyard)
      ((eq current :lumberyard) :open)
      (t current))))

;; ** Calculate the area after one time step.

(defun effect-change (area)
  "Returns the AREA after one minute."
  (iter
    (with new-area = (make-array (array-dimensions area)
                                 :initial-element :open))
    (for y below (array-dimension area 0))
    (iter (for x below (array-dimension area 1))
          (setf (aref new-area y x) (new-value area x y)))
    (finally (return new-area))))

;; ** Calculate the area 

(defun effect-changes (area count)
  "Returns the AREA after COUNT time steps. I am using a hash to check if a
state has already been seen."
  (iter
    (with states-seen = (make-hash-table :test 'equalp))
    (for i from 0 to count)
    (for a first area then (effect-change a))
    ;; Calculate the frequency with that the states are repeating
    (when (gethash a states-seen)
          (let* ((base (gethash a states-seen))
                 (offset (mod (- count base) (- i base))))
            (leave (effect-changes a offset)))) ; Returning without further calculation
    (setf (gethash a states-seen) i)
    (finally (return a))))

(defun resource-value (area)
  (* (count :trees (flatten-array area))
     (count :lumberyard (flatten-array area))))

(defun aoc-18a ()
  "Returns the answer for the first part of day 18."
  (-<> (read-18)
       (parse-input <>)
       (effect-changes <> 10)
       (resource-value <>)))

;; * Part 2

(defun aoc-18b ()
  "Returns the answer for the second part of day 18."
  (-<> (read-18)
       (parse-input <>)
       (effect-changes <> 1000000000)
       (resource-value <>)))

;; * Tests

(define-test test-18
  (assert-equal 1147 (resource-value (effect-changes *test-area* 10))))

