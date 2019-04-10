;;;; * Day 8
;;; The package
(in-package :aoc-18)

;;; ** A simple data structure
(defstruct node
  "A NODE has its DATA and its CHILDREN."
  (data nil)
  (children nil))

;;; ** Test data
(defparameter *test-input-8*
  '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))

(defparameter *test-input-8.1*
  '(2 4 1 3 0 1 8 10 11 12 2 1 0 1 99 0 1 7 2 1 1 2 4))

;;; ** Reading the input data
(defun read-input8 (&optional (input #p"inputs/input8.txt"))
  "Reads the input for AOC-18 day 8 and returns it as a list of integers."
  (-<> (read-file-into-string input)
       (split "\\s" <>)
       (mapcar #'parse-integer <>)))

;;; ** Building the tree
;;; Auxiliary function to split a list as defined here
;;; https://github.com/MartinBuchmann/99-lisp-problems/blob/master/17.lisp
(defun split-at (count original-list)
  (unless (or (not (listp original-list)) (null original-list) (minusp count))
    (list (subseq original-list 0 count)
          (subseq original-list count))))

;;; Idea from:  https://gitlab.com/asciiphil/advent-of-code/blob/master/2018/08.lisp
(defun parse-license (input)
  "Takes the list of the INPUT license file and returns a tree structure of the nodes."
  (if (null input)
      (values nil nil) ; Using values to handle the remaining input
      (destructuring-bind ((child-count data-count) input-without-header)
          (split-at 2 input) ; Get the number of children and data
        (destructuring-bind (children input-without-children)
            (if (<= child-count 0) ; No children --> return the input without change
                (list nil input-without-header)
                (iter (repeat child-count) ; For each child remove the data
                  (for (values new-child remaining-input)
                       first (parse-license input-without-header)
                       then (parse-license remaining-input))
                  (collecting new-child into children)
                  (finally (return (list children remaining-input)))))
          (destructuring-bind (data input-without-node) ; Store the data in the node
              (split-at data-count input-without-children)
            (values (make-node :data data ; Return the current node and the remaining input
                               :children children)
                    input-without-node))))))

;;; ** Finding the meta data

;;; Instead of building the tree structure I tried to use a rather
;;; simple approach to get the meta data from the initial list. The
;;; iteration structure is quite complex but straight-forward.
;;;
;;; Depending on the number of children the meta data will be
;;; extracted from the initially list and the processed data will be
;;; cut off from the list using subseq.
;;;
;;; Of course, the summing of the data could have been implemented here
;;; also.
;;;
;;; Unfortunately, it works fine only for the test but not for the
;;; real data...

;;; ... because I missed the most important point to handle the
;;; children correctly! I tried to add this but the iteration gets so
;;; complex that I do not like the code any more. The version above
;;; works so elegantly that I will stick with it but leave my
;;; erroneous approach for later study.
(defun find-metadata% (input)
  "Returns a list containing the meta data, only."
  (iter
    (for number-of-children first (nth 0 input) then (nth 0 remaining-input))
    (for data-count first (nth 1 input) then (nth 1 remaining-input))
    (for len first (length input) then (length remaining-input))
    (assert (plusp data-count) (remaining-input) "No data? ~S" remaining-input)
    (for data-start = (if (zerop number-of-children) 2 (- len data-count)))
    (for data-end = (+ data-start data-count))
    (for data first (subseq input data-start data-end)
         then (subseq remaining-input data-start data-end))
    (for remaining-input first (subseq input 2 data-start)
         ;; Without children in the first place, there would be nothing.
         then (if (zerop number-of-children)
                  (subseq remaining-input data-end)
                  (subseq remaining-input 2 data-start)))
    ;; (when (not (zerop number-of-children))
    ;;   (find-metadata% remaining-input)) ; This is not sufficient!
    ;; The cutting of the data of each child should be implemented
    ;; without recursion.
    (dbg :08-fm "n: ~D d-c; ~2D S: ~5D E: ~5D len: ~5D D: ~A~%"
         number-of-children data-count data-start data-end len data)
    (appending data)
    (while remaining-input)))

(defun find-metadata%% (input)
  (if (null input)
      (values nil nil)
      (destructuring-bind ((child-count data-count) input-wo-header)
          (split-at 2 input)
        (destructuring-bind (children input-wo-children)
            (if (zerop child-count)
                (list nil input-wo-header)
                (iter (repeat child-count)
                  (for (values new-child remaining-input)
                       first (find-metadata%% input-wo-header)
                       then (find-metadata%% remaining-input))
                  (collecting new-child into children)
                  (finally (return (list children remaining-input)))))
          (destructuring-bind (data input-wo-node)
              (split-at data-count input-wo-children)
            (dbg :08-fm%% "D: ~D wo-node: ~A~%" data input-wo-node)
            (values (make-node :data data :children children) input-wo-node))))))

;;; * Part 1

(defun sum-metadata (license-tree)
  "Returns the sum of all meta date from the LICENSE-TREE."
  (if (null license-tree)
      0
      (+ (reduce #'+ (node-data license-tree))
         (reduce #'+ (mapcar #'sum-metadata (node-children license-tree))))))

(defun aoc-08a ()
  "My solution of the first part of day 8."
  (-> (read-input8)
      (parse-license)
      (sum-metadata)))

;;; * Part 2

(defun root-node-value (license-tree)
  "Returns the value of the root node, as described in the puzzle's
description."
  (cond
    ((null license-tree) 0)
    ((endp (node-children license-tree))
     (apply #'+ (node-data license-tree)))
    (t (let ((child-sums (mapcar #'root-node-value (node-children license-tree))))
         (reduce #'+ (mapcar (lambda (v)
                               (or (nth (1- v) child-sums) 0))
                             (node-data license-tree)))))))

(defun aoc-18-08b ()
  "My solution of the second part of day 8."
  (-> (read-input8)
      (parse-license)
      (root-node-value)))

(define-test test-08
  (assert-equal 138 (reduce #'+ (find-metadata% *test-input-8*)))
  (assert-equal 138 (sum-metadata (parse-license *test-input-8*)))
  (assert-equal 66 (root-node-value (parse-license *test-input-8*))))
