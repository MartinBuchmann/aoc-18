(in-package :aoc-18)

(defun read-input6 (&optional (input #p "inputs/input6.txt"))
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

;;; Vielleicht gar nicht notwendig
(defun initialize-area (max-x max-y coordinates)
  (iter
    (with area = (make-array (list (1+ max-x) (1+ max-y))
                             :element-type 'unsigned-byte :initial-element 0))
    (for (x y) in coordinates)
    (for i from 1)
    (setf (aref area x y) i)
    (log:info "I: ~D X: ~D Y: ~D" i x y)
    (finally (return area))))

;;; TODO: Manhattan-Distanz berechen, Iterate statt loop verwenden
(defun minimum-manhattan-distance (x y coordinates)
  (loop :for c :in coordinates
	:for i :from 0
	:for dx := (abs (- (first c) x))
	:for dy := (abs (- (second c) y))
	:collect (list i (+ dx dy)) :into results
	:finally (return
		   (let ((sorted (sort (copy-seq results) #'< :key #'second)))
		     (if (= (cadar sorted) (cadadr sorted))
			 nil
			 (caar sorted))))))

;;; TODO: Unendliche Bereiche identifizieren
(defun detect-infinites (coordinates)
  "Return a hash-table with coordinates' cardinals for areas that extend infinitely."
  (flet ((mark-inf (x y coordinates ht)
	   (let ((id (minimum-manhattan-distance x y coordinates)))
	     (unless (or (null id) (gethash id ht))
	       (setf (gethash id ht) t)))))
    (let ((infinites (make-hash-table)))
      (destructuring-bind (max-x max-y) (max-dimensions coordinates)
	(loop :for x :in (list 0 max-x)
	      :do (loop :for y :from 0 :upto max-y
			:do (mark-inf x y coordinates infinites)))
	(loop :for y :in (list 0 max-y)
	      :do (loop :for x :from 1 :upto (1- max-x)
			:do (mark-inf x y coordinates infinites))))
      infinites)))

;;; TODO: Hauptroutine implementieren
(multiple-value-bind (max-x max-y coordinates)
    (read-input6)
  (initialize-area max-x max-y coordinates))
