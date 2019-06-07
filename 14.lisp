;;; -*- ispell-local-dictionary: "en" -*-
;;; * Day 14
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;;; * My input

(defparameter *day-14-limit* "030121" "My input for day 14.")

;;; * A new data structure for the scoreboard
(defstruct board
  "A data structure for the scoreboard.

- scores is an adjustable array with all the recipes so far.
- elves is a two element list with the positions of the two elves."
  (scores (make-array 2
                      :element-type '(integer 0 9)
                      :initial-contents '(3 7)
                      :adjustable t
                      :fill-pointer t))
  (elves (list 0 1)))

;;; ** A helper function used during implementation
(defun print-board (board)
  "Prints the current score board."
  (with-slots (scores elves) board
    (iter
      (for score in-vector scores with-index i)
      (format t (cond
                  ((= i (first elves))
                   "(~A) ")
                  ((= i (second elves))
                   "[~A] ")
                  (t
                   "~A "))
              score))
    (terpri)))

;;; * Part 1

(defun next-recipe (board)
  "Returns an updated BOARD based on the scores of the current
recipes, i.e. the position of the elves."
  (with-slots (scores elves) board
    (let ((new-recipe (+ (aref scores (first elves))
                         (aref scores (second elves)))))
      (if (< new-recipe 10)
          (vector-push-extend new-recipe scores)
          (multiple-value-bind (new-recipe-1 new-recipe-2)
              (truncate new-recipe 10)
            (vector-push-extend new-recipe-1 scores)
            (vector-push-extend new-recipe-2 scores)))
      (setf elves (mapcar (lambda (pos)
                            (mod (+ pos 1 (aref scores pos)) (length scores)))
                          elves))))
  board)

(defun next-scores (n)
  "Returns the next 10 scores as an vector of digits after N recipes have been made."
  (let ((board (make-board))
        (recs (+ n 10)))
    (labels ((make-recipes (board recipes-to-make)
               (if (<= recipes-to-make (length (board-scores board)))
                   board
                   (make-recipes (next-recipe board) recipes-to-make))))
      (subseq (board-scores (make-recipes board recs)) n recs))))

(defun aoc-14a ()
  "Returns the answer for the first part of day 14 as a string."
  (map 'string #'digit-char
       (next-scores (parse-integer *day-14-limit*))))

;;; * Part 2

(defun string->digitvector (string)
  "Return a vector of digits for the STRING of digits. Assuming valid input."
  (map 'vector #'digit-char-p string))

(defun number-of-recipes (recipes board &aux (pattern (string->digitvector recipes)))
  "Returns the number of recipes needed before the pattern of RECIPES
scores (given as a string) is found on the BOARD."
  (with-slots (scores) board
    (labels ((find-recipes (pattern board)
               (let ((len-score   (length scores))
                     (len-pattern (length pattern)))
                 (if (and (< len-pattern len-score) ; Not enough recipes so far
                          ;; Because in each step one or two new recipes can be
                          ;; created, we have to check both cases.
                          (or (equalp pattern (subseq scores (- len-score len-pattern)))
                              (equalp pattern (subseq scores (- len-score len-pattern 1) (1- len-score)))))
                     ;; We found the pattern and return the length, i.e. the number of recipes
                     (length (subseq (board-scores board) 0 (search pattern scores)))
                     ;; Not found yet, carry on...
                     (find-recipes pattern (next-recipe board))))))
      (find-recipes pattern board))))

(defun aoc-14b ()
  "Returns the answer of the second part of day 14."
  (number-of-recipes *day-14-limit* (make-board)))

;;; * Tests
(define-test test-14
  (assert-equalp #(5 1 5 8 9 1 6 7 7 9) (next-scores 9))
  (assert-equalp #(0 1 2 4 5 1 5 8 9 1) (next-scores 5))
  (assert-equalp #(9 2 5 1 0 7 1 0 8 5) (next-scores 18))
  (assert-equalp #(5 9 4 1 4 2 9 8 8 2) (next-scores 2018))
  (assert-equal 9 (number-of-recipes "51589" (make-board)))
  (assert-equal 5 (number-of-recipes "01245" (make-board)))
  (assert-equal 18 (number-of-recipes "92510" (make-board)))
  (assert-equal 2018 (number-of-recipes "59414" (make-board))))

