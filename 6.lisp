(in-package :aoc-18)
(annot:enable-annot-syntax)

(defun read-input6 (&optional (input #p "inputs/input6.txt"))
  "Returns the Number of given coordinates, the bounding box of the area and the
list of coordinates."
  (iter
    (with max-x = 0) (with max-y = 0)
    (for line in-file input using #'read-line)
    (for (x y) = (register-groups-bind ((#'parse-integer x y))
                 ("(\\d+)\\,\\s(\\d+)" line)
                   (list x y)))
    (when (> x max-x) (setf max-x x))
    (when (> y max-y) (setf max-y y))
    (collect (list x y) into coordinates)
    (finally (return (values max-x max-y coordinates)))))

(defun minimum-manhattan-distance (x y coordinates)
  "Returns the coordinate which has the lowest manhatten distance from X Y or
NIL if two coordinates lead to a minimum."
  (iter
    (with min-d = most-positive-fixnum)
    (with min-i = nil)
    (for c in coordinates)
    (for i from 0)
    (for dx = (abs (- (first c)  x)))
    (for dy = (abs (- (second c) y)))
    (cond ((< #1=(+ dx dy) min-d) (setf min-d #1# min-i i))
          ((= #1# min-d) (setf min-i nil)))
    (finally (return min-i))))

;;; For Debugging only
(defun print-area ()
  (multiple-value-bind (max-x max-y coordinates)
      (read-input6 "tests/test-06.txt")
    (iter (for x from 0 to max-x)
          (iter (for y from 0 to max-y)
                (aif (minimum-manhattan-distance y x coordinates)
                     (princ it)
                     (princ #\.)))
          (terpri))))

;;; The coordinates which have a minimum distance on the outer bound will extend
;;; towards infinity.
(defun detect-infinites (max-x max-y coordinates &aux (infinites (make-hash-table)))
  "Return a hash-table with coordinates that have an infinit area."
  (flet ((mark-inf (x y coordinates ht &aux (id (minimum-manhattan-distance x y coordinates)))
	   (unless (or (null id) (gethash id ht))
	     (setf (gethash id ht) t))))
    (iter (for x in (list 0 max-x))
	  (iter (for y from 0 to max-y)
		(mark-inf x y coordinates infinites)))
    (iter (for y in (list 0 max-y))
	  (iter (for x from 1 to (1- max-x))
		(mark-inf x y coordinates infinites)))
    infinites))

(defun area-sizes (max-x max-y coordinates
                   &aux (num-of-coor (length coordinates))
                     (infinites (detect-infinites max-x max-y coordinates))
                     (result (make-array num-of-coor :element-type 'fixnum
                                                     :initial-element 0)))
  "Returns an array with the area size of the coordinates or 0 if the area is
infinit."
  (iter (for x from 0 to max-x)
        (iter (for y from 0 to max-y)
              (for id = (minimum-manhattan-distance x y coordinates))
              (unless (or (null id) (gethash id infinites))
                (incf (aref result id)))))
   result)

(defun aoc18-06a (&optional (input #p"inputs/input6.txt"))
  (multiple-value-bind (max-x max-y coordinates)
      (read-input6 input)
    (iter (for area in-vector (area-sizes max-x max-y coordinates))
          (maximize area))))

;;; Valid only for my input
(define-test test-06
  (assert-equal 357 (nth-value 0 (read-input6)))
  (assert-equal 356 (nth-value 1 (read-input6)))
  (assert-equal 17 (aoc18-06a "tests/test-06.txt")))

(multiple-value-bind (max-x max-y coordinates)
    (read-input6 "tests/test-06.txt")
  @ignore max-x max-y
  (minimum-manhattan-distance 0 0 coordinates))

(multiple-value-bind (max-x max-y coordinates)
    (read-input6 "tests/test-06.txt")
  (detect-infinites max-x max-y coordinates))

(multiple-value-bind (max-x max-y coordinates)
    (read-input6 "tests/test-06.txt")
  (area-sizes max-x max-y coordinates))
