;;; -*- ispell-local-dictionary: "en_GB" -*-
;;;; aux-fns.lisp
;;; Time-stamp: <2019-06-22 22:32:39 Martin>
;;;
;;; Auxiliary functions for my AOC 2018 solutions
;;; Debugging functions from Peter Norvig's PAIP

(in-package :aoc-18)
(annot:enable-annot-syntax)

;;; * Debugging helpers
(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun start-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

;;; * Acessing array elements specified by complex numbers
(defun cref (array complex-number)
  "Returns the value of the 2d ARRAY at the position given as a COMPLEX NUMBER."
  (aref array (imagpart complex-number) (realpart complex-number)))

(defun (setf cref) (new-value array complex-number)
  "Sets the value of the 2d ARRAY at the position given as a COMPLEX NUMBER to NEW-VALUE."
  (setf (aref array (imagpart complex-number) (realpart complex-number)) new-value))

;;; ** Shortest Path Taken from Phil! Gold.
;;;
;;; Using cl-containers to implement a priority queue for all available options
;;; (nodes in a search tree).
(defun find-shortest-path (finishedp next-options visited options test)
  "Returns a list of nodes (cost from-node to-node) for the the shortest-paths
in OPTIONS according to TEST.  VISITED is a hash with a list of the nodes
visited so far to get to-node."
  (if (cl-containers:empty-p options) 
      (values nil nil)
      (destructuring-bind (cost from-node to-node) (cl-containers:delete-first options)
        (if (gethash to-node visited)
            (find-shortest-path finishedp next-options visited options test)
            (progn
              (setf (gethash to-node visited) (cons to-node (gethash from-node visited)))
              (if (funcall finishedp to-node)
                  (values (reverse (gethash to-node visited))
                          cost)
                  (let ((next-edges (remove-if (lambda (edge) (gethash (second edge) visited))
                                               (funcall next-options to-node))))
                    (iter (for (next-cost next-node) in next-edges)
                          (cl-containers:insert-item options
                                                     (list (+ cost next-cost)
                                                           to-node
                                                           next-node)))
                    (find-shortest-path finishedp next-options visited options test))))))))

(defun shortest-path (start next-options &key end finishedp (test 'eql) heuristic)
  "Finds the shortest path from START to END.  NEXT-OPTIONS should be a
  function that accepts a state and returns a list of `(cost state)` pairs
  signifying next moves from the given state.  Returns two values: a list
  of states from START to END, and the total cost of the path.  TEST
  determines how the states are compared and should be usable as a hash
  table test.  The optional HEURISTIC is a function that is called with a
  state and returns an estimate of the minimum cost from that state to
  END.

  In place of END, you can give FINISHEDP, which is a function that will
  be called on each state.  It should return true if the state is the end
  of the path and false otherwise."
  (when (and (not end)
             (not finishedp))
    (error "Must give either END or FINISHEDP."))
  (when (and end finishedp)
    (error "Cannot give both END and FINISHEDP."))
  (let ((real-finishedp (or finishedp
                            (lambda (state) (funcall test state end)))))
    (if (funcall real-finishedp start)
        (values (list start) 0)
        (let ((visited (make-hash-table :test test))
              (options (make-instance 'cl-containers:priority-queue-on-container
                                      :key (if heuristic
                                               (lambda (edge)
                                                 (destructuring-bind (cost from-node to-node) edge
                                                   (declare (ignore from-node))
                                                   (+ cost (funcall heuristic to-node))))
                                               #'first)
                                      :test (lambda (a b)
                                              (and (= (first a) (first b))
                                                   (funcall test (second a) (second b))
                                                   (funcall test (third a) (third b))))
                                      :sorter #'<)))
          (iter (for (cost node) in (funcall next-options start))
                (cl-containers:insert-item options (list cost start node)))
          (setf (gethash start visited) (list start))
          (find-shortest-path real-finishedp next-options visited options test)))))
