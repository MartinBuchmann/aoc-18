;;; -*- ispell-local-dictionary: "en_GB" -*-
;; * Day 17
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; * The data structure for the scan

(defstruct scan
  "The data of the scan."
  offset-x
  offset-y
  grid)

(defmethod print-object ((scan scan) stream)
  (with-slots (offset-x offset-y grid) scan
    (format stream "#<SCAN (~A,~A)~%" offset-x offset-y)
    (iter (for y from 0 below (array-dimension grid 0))
      (format stream "  ")
      (iter (for x from 0 below (array-dimension grid 1))
        (format stream "~A"
                (ecase (aref grid y x)
                  (:sand #\░)
                  (:clay #\█)
                  (:water #\▓)
                  (:wet-sand #\▒))))
      (format stream "~%"))
    (format stream "  >")))

(defun scan-ref (scan x y)
  "Returns the grid value of SCAN with respect to the given offset."
  (aref (scan-grid scan)
        (- y (scan-offset-y scan))
        (- x (scan-offset-x scan))))

(defsetf scan-ref (scan x y) (new-value)
  (let ((scan-value (gensym "SCAN")))
    `(let ((,scan-value ,scan))
       (setf (aref (scan-grid ,scan-value)
                   (- ,y (scan-offset-y ,scan-value))
                   (- ,x (scan-offset-x ,scan-value)))
             ,new-value))))

(defun scan-max-y (scan)
  "Returns the largest y value of the scan."
  (1- (+ (scan-offset-y scan) (array-dimension (scan-grid scan) 0))))

;;; * Extracting the input

(defun read-17 (&optional (input #p"inputs/input17.txt"))
  "Returns the input of day 17 as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))

;; * Parsing the scan

(defun parse-input (input)
  "Parses the input and returns an scan object with the offset of the
upper left corner and an array of the formation."
  (labels ((expand-line (groups minx maxx miny maxy)
             (list (list (parse-integer (svref groups minx))
                         (1+ (parse-integer (svref groups maxx))))
                   (list (parse-integer (svref groups miny))
                         (1+ (parse-integer (svref groups maxy))))))
           (make-scan-from-coords (coord-list)
             (iter (for ((x0 x1) (y0 y1)) in coord-list)
               (minimizing x0 into minx)
               (maximizing x1 into maxx)
               (minimizing y0 into miny)
               (maximizing y1 into maxy)
               (finally
                (return
                  (make-scan :offset-x (1- minx)
                             :offset-y miny
                             :grid (make-array
                                    (list (- maxy miny)
                                          (- maxx minx -2))
                                    :initial-element :sand))))))
           (fill-scan (coord-list)
             (let ((scan (make-scan-from-coords coord-list)))
               (iter (for ((minx maxx) (miny maxy)) in coord-list)
                 (iter (for x from minx below maxx)
                   (iter (for y from miny below maxy)
                     (setf (scan-ref scan x y) :clay))))
               scan)))
    (fill-scan
     (iter (for line in input)
       (for groups = (nth-value 1 (ppcre:scan-to-strings
                                   "^([xy])=(\\d+), ([xy])=(\\d+)\\.\\.(\\d+)$"
                                   line)))
       (collecting (if (string= (svref groups 0) "x")
                       (expand-line groups 1 1 3 4)
                       (expand-line groups 3 4 1 1)))))))

;; * The input and test parameters

(defparameter *test-scan*
  '("x=495, y=2..7"
    "y=7, x=495..501"
    "x=501, y=3..7"
    "x=498, y=2..4"
    "x=506, y=1..2"
    "x=498, y=10..13"
    "x=504, y=10..13"
    "y=13, x=498..504"))

;; * Part 1

;; * Add the water from the source

(defun add-water (scan x y)
  "Add water to the coordiante X Y of SCAN and check if the water can flow further."
  (setf (scan-ref scan x y) :wet-sand)
  (cond
    ((or (<= (scan-max-y scan) y)
         (eq :wet-sand (scan-ref scan x (1+ y))))
     scan)
    ((eq :sand (scan-ref scan x (1+ y)))
     (add-water scan x (1+ y)))
    ((or (eq :water (scan-ref scan x (1+ y)))
         (eq :clay (scan-ref scan x (1+ y))))
     (add-water-row scan x y))
    (t
     (assert nil
             ()
             "Unknown conditions: (~A,~A) ~A" x y scan))))

(defun add-water-row (scan x y)
  "Adds water to the current row X of SCAN."
  (labels ((find-bounds (inc)
             (iter (for xp from x by inc)
               (while (member (scan-ref scan xp (1+ y)) '(:clay :water)))
               (when (eq (scan-ref scan (+ xp inc) y) :clay)
                 (leave (list xp nil)))
               (finally (return (list xp t))))))
    (destructuring-bind (left-bound left-flow) (find-bounds -1)
      (destructuring-bind (right-bound right-flow) (find-bounds 1)
        (iter (for xp from left-bound to right-bound)
          (setf (scan-ref scan xp y)
                (if (or left-flow right-flow)
                    :wet-sand
                    :water)))
        (when left-flow
          (add-water scan left-bound y))
        (when right-flow
          (add-water scan right-bound y))
        (when (not (or left-flow right-flow))
          (add-water-row scan x (1- y)))
        scan))))

(defun fill-water (scan)
  "Fill the scan with water from the source."
  (add-water scan 500 (scan-offset-y scan)))

(defun count-matching-tiles (scan tile-types)
  "Returns the number of tiles which are part of TILES-TYPES."
  (count-if (lambda (tile) (member tile tile-types))
            (make-array (array-total-size (scan-grid scan))
                        :displaced-to (scan-grid scan))))

(defun aoc-17a ()
  "Returns the answer for the first part of day 17."
  (-<> (read-17)
       (parse-input <>)
       (fill-water <>)
       (count-matching-tiles <> '(:water :wet-sand))))

;; * Part 2

(defun aoc-17b ()
  "Returns the answer of the second part of day 17."
  (-<> (read-17)
       (parse-input <>)
       (fill-water <>)
       (count-matching-tiles <> '(:water))))

;; * Tests

(define-test test-17
  (assert-equal 57 (count-matching-tiles
                    (fill-water (parse-input *test-scan*))
                    '(:water :wet-sand)))
  (assert-equal 29 (count-matching-tiles
                    (fill-water (parse-input *test-scan*))
                    '(:water)))
  (assert-equal 30384 (aoc-17a))
  (assert-equal 24479 (aoc-17b)))
