;; -*- ispell-local-dictionary: "en" -*-
;;;; * Day 11
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; ** Test data
(defparameter *11-test-grid-1*
  #2A((-2 -4  4  4  4)
      (-4  4  4  4 -5)
      ( 4  3  3  4 -4)
      ( 1  1  2  4 -3)
      (-1  0  2 -5 -2)))

(defparameter *11-test-grid-2*
  #2A((-3  4  2  2  2)
      (-4  4  3  3  4)
      (-5  3  3  4 -4)
      ( 4  3  3  4 -3)
      ( 3  3  3 -5 -1)))

;; ** Calculating the power level.
@inline
(defun rack-id (x)
  "A simple wrapper for the rack id of coordinate X."
  @type fixnum x
  (the fixnum (+ x 10)))

@inline
(defun hundreds-digit (n)
  "Returns the hundreds digit of N."
  @type fixnum n
  (if (< n 100)
      0
      (iter
        (for d from 1 to 3)
        (for (values remaining last) first (floor n 10) then (floor remaining 10))
        (when (< 0 remaining))
        (finally (return last)))))

(defun power-level (x y serial &aux (rack-id (rack-id x)))
  "Calulates the power level of coordinate (X,Y) and SERIAL."
  @type fixnum x y serial
  (the fixnum
       (- (hundreds-digit (* rack-id (+ (* y rack-id) serial))) 5)))

;; ** Calulate the power squares
(defun power-square (grid &optional (size 3))
  "Returns the sum of the power values for each SIZExSIZE square of GRID."
  (iter
    (with (x-size y-size) = (array-dimensions grid))
    (with result = (make-array (list (- x-size (1- size)) (- y-size (1- size)))
                               :element-type 'fixnum :initial-element 0))
    (for i from 0 to (- x-size size))
    (iter
      (for j from 0 to (- y-size size))
      (iter
        (for k from 0 to (1- size))
        (iter
          (for l from 0 to (1- size))
          (incf (aref result i j) (aref grid (+ i k) (+ j l))))))
    (finally (return result))))

;; ** Calculate the initial grid
(defun calculate-grid (serial &optional (size 300))
  "Calculate the grid of power values for SERIAL."
  (iter
    (with result = (make-array (list size size) :element-type 'fixnum :initial-element 0))
    (for i from 0 below size)
    (iter
      (for j from 0 below size)
      (setf (aref result i j) (power-level i j serial)))
    (finally (return result))))

;; ** Finding the maximu power value
(defun maximum-power-square (power-squares)
  "Returns the coordinates of the top left corner of the largest POWER-SQUARE."
  (iter outer
    (with (x-size y-size) = (array-dimensions power-squares))
    (for i from 0 below x-size)
    (iter
      (for j from 0 below y-size)
      (for fuel-sum = (aref power-squares i j))
      (in outer (finding (list i j fuel-sum) maximizing fuel-sum)))))

;; * Part one
(defun aoc-11a (&optional (serial 1309))
  (destructuring-bind (x y power)
      (maximum-power-square (power-square (calculate-grid serial)))
    (format t "The maximum power cell (~D) starts at: ~D,~D~%" power x y)
    (list x y power)))

;; * Part two

;; ** Summed array tables
;;
;; Ich verwende /summed-array-tables/ um die Power-Werte zu berechnen.
;; https://de.wikipedia.org/wiki/Integralbild und
;; https://en.wikipedia.org/wiki/Summed-area_table

(defun summed-area-table (array
                          &aux (x-size (array-dimension array 0))
                            (y-size (array-dimension array 1))
                            (table (make-array (list x-size y-size))))
  "Returns the summed area table of ARRAY."
  (iter
    (for x below x-size)
    (iter
      (for y below y-size)
      (setf (aref table x y)
            (+ (aref array x y)
               (if (< 0 x)
                   (aref table (1- x) y)
                   0)
               (if (< 0 y)
                   (aref table x (1- y))
                   0)
               (if (and (< 0 x) (< 0 y))
                   (- (aref table (1- x) (1- y)))
                   0)))))
  table)

(defun summed-area-table-get-sum (table x y width &optional (height width))
  "Returns the sum for X,Y with WIDTH,HEIGHT from TABLE."
  (let ((upper-x (+ x width -1))
        (upper-y (+ y height -1))
        (lower-x (1- x))
        (lower-y (1- y)))
    (+ (aref table upper-x upper-y)
       (- (if (not (minusp lower-x))
              (aref table lower-x upper-y)
              0))
       (- (if (not (minusp lower-y))
              (aref table upper-x lower-y)
              0))
       (if (and (not (minusp lower-x))
                (not (minusp lower-y)))
           (aref table lower-x lower-y)
           0))))

(defun power-square-using-summed-tables (dimension summed-table) 
  (iter (with in-size = (array-dimension summed-table 0))
        (with out-size = (1+ (- in-size dimension)))
        (with result = (make-array (list out-size out-size)))
        (for x from 0 below out-size)
        (iter (for y from 0 below out-size)
              (setf (aref result x y)
                    (summed-area-table-get-sum summed-table x y dimension)))
        (finally (return result))))

(defun maximum-power-square-and-size (grid)
  "Returns the coordinates and the size of the largest POWER-SQUARE."
  (iter
    (with summed-table = (summed-area-table grid))
    (for size from 1 to (array-dimension grid 0))
    (for (x y fuel-sum) = (maximum-power-square
                           (power-square-using-summed-tables size summed-table)))
    (finding (list x y size fuel-sum) maximizing fuel-sum)))

(defun aoc-11b (&optional (serial 1309))
  (destructuring-bind (x y size power)
      (maximum-power-square-and-size (calculate-grid serial))
    (format t "The maximum power cell (~D) starts at: ~D,~D with size: ~D~%" power x y size)
    (format t "~D,~D,~D" x y size)
    (list x y size power)))

(define-test test-11
  (assert-equal  4 (power-level 3 5 8))
  (assert-equal -5 (power-level 122 79 57))
  (assert-equal  0 (power-level 217 196 39))
  (assert-equal  4 (power-level 101 153 71))
  (assert-equalp '(1 1 29) (maximum-power-square (power-square *11-test-grid-1*)))
  (assert-equalp '(1 1 30) (maximum-power-square (power-square *11-test-grid-2*)))
  (assert-equalp '(20 43 31) (aoc-11a))
  (assert-equalp '(233 271 13 108) (aoc-11b)))

