(in-package :aoc-18)
(annot:enable-annot-syntax)

(defun read-input7 (&optional (input #p"inputs/input7.txt"))
  "Reads the input for AOC-18 day 7."
  (iter
    (for line in-file input using #'read-line)
    (collect (register-groups-bind (a b)
                 ("Step (\\w) must be finished before step (\\w) can begin." line)
               (list a b)))))

(defun instruction-graph (instructions &aux (graph (digraph:make-digraph :test #'equal)))
  (iter
    (for (a b) in instructions)
    (digraph:insert-vertex graph a)
    (digraph:insert-vertex graph b)
    (digraph:insert-edge graph a b)
    (dbg :07 ";;; ~A ~A~%" a b))
  (digraph.dot:draw graph :filename "graph.png" :format :png)
  graph)

(defun sort-graph (graph &aux (result (make-array 0
                                        :element-type 'character
                                        :fill-pointer 0
                                        :adjustable t)))
  (iter
    (for s in (digraph:topological-sort graph))
    (vector-push-extend (coerce s 'character) result))
  result)

(digraph:count-vertices *instructions*)
(digraph:count-edges *instructions*)

(defun aoc18-07a (&optional (input #p"inputs/input7.txt"))
  "The first part of day 7."
  (-> input
      (read-input7)
      (instruction-graph)
      (sort-graph)
      (nreverse)))

;;; TSLGXBINDKVPJMUFCAHEOWYQRZ

