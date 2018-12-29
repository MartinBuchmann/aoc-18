(in-package :aoc-18)

(defun get-coordinates (input)
  "Returns the coordinates parsed from the string INPUT as defined by the AOC18 puuzle from day 3.

#1 @ 11,3: 4x4 --> 11 3 15 7"
  (let*
      ((pos-colon (position #\: input))
       (pos-comma (position #\, input :from-end t :end pos-colon))
       (pos-space (position #\space input :from-end t :end pos-comma))
       (pos-x (position #\x input))
       (x1 (parse-integer (subseq input (1+ pos-space) pos-comma)))
       (y1 (parse-integer (subseq input (1+ pos-comma) pos-colon)))
       (dx (parse-integer (subseq input (1+ pos-colon) pos-x)))
       (dy (parse-integer (subseq input (1+ pos-x)))))
    (values x1 y1 (+ x1 dx) (+ y1 dy))))

(defun aoc18-03a (&key (input #p"inputs/input3.txt") (size 1000)
                  &aux (fabric
                        (make-array (list size size) :element-type 'unsigned-byte :initial-element 0)))
  "Solves the first puzzle of day 3."
  ;; First we will parse the input and fill the fabric accordingly.
  (iter
    (for line in-file input using #'read-line)
    (for (values a b c d) = (get-coordinates line))
    (iter
      (for x :from a below c)
      (iter 
        (for y :from b below d)
        (incf (aref fabric x y)))))
  ;; Now let's analyse how many fields are used repeatedly.
  (iter
    (for i :from 0 below (array-total-size fabric))
    (when (< 1 (row-major-aref fabric i)) (count i))))


(defun get-coordinates-and-id (input)
  "Returns the coordinates parsed from the string INPUT as defined by the AOC18 puuzle from day 3.

#1 @ 11,3: 4x4 --> 1 11 3 15 7"
  (let*
      ((pos-at (position #\@ input))
       (pos-colon (position #\: input))
       (pos-comma (position #\, input :from-end t :end pos-colon))
       (pos-space (position #\space input :from-end t :end pos-comma))
       (pos-x (position #\x input))
       (id (parse-integer (subseq input 1 pos-at)))
       (x1 (parse-integer (subseq input (1+ pos-space) pos-comma)))
       (y1 (parse-integer (subseq input (1+ pos-comma) pos-colon)))
       (dx (parse-integer (subseq input (1+ pos-colon) pos-x)))
       (dy (parse-integer (subseq input (1+ pos-x)))))
    (values id x1 y1 (+ x1 dx) (+ y1 dy))))

(defun invalidate-ids (id1 id2 ids fabric x y)
  "Sets all fields of FABRIC with ID to 'X."
  (setf (aref fabric x y) 'X)
  (setf (gethash id1 ids) -1)
  (setf (gethash id2 ids) -1))

(defun aoc18-03b (&key (input #p"inputs/input3.txt") (size 1000)
                  &aux (fabric (make-array (list size size) :initial-element nil))
                       (ids (make-hash-table)))
  "Solves the second puzzle of day 3."
  ;; First we will parse the input and fill the fabric accordingly.
  (iter
    (for line in-file input using #'read-line)
    (for (values id a b c d) = (get-coordinates-and-id line))
    (setf (gethash id ids) 0)
    (iter
      (for x :from a below c)
      (iter 
        (for y :from b below d)
        (if (null #1=(aref fabric x y))  ; The field is empty
            (setf #1# id)
            (invalidate-ids id #1# ids fabric x y)))))
  ;; Now let's analyse how many fields are used repeatedly.
  (iter
    (for (key value) in-hashtable ids)
    (when (zerop value) (return key))))

(define-test test-03
  (assert-equal 118858 (aoc18-03a))
  (assert-equal 1100 (aoc18-03b)))
