(in-package :aoc-18)
(annot:enable-annot-syntax)

(defun read-input7 (&optional (input #p"inputs/input7.txt"))
  "Reads the input for AOC-18 day 7."
  (iter
    (for line in-file input using #'read-line)
    (collect (register-groups-bind (a b)
                 ("Step (\\w) must be finished before step (\\w) can begin." line)
               (list a b)))))

(defun instructions (steps &aux (instructions (make-hash-table :test #'equal)))
  "Returns a hashtable where the keys B are the dependents of the values A."
  (iter
    (for (a b) in steps)
    ;; Adding the steps
    (when (not #1=(gethash a instructions))
      (setf #1# '()))
    ;; Comleting the hash
    (if (nth-value 1 #2=(gethash b instructions))
        (setf #2# (cons a #2#))
        (setf #2# (list a))))
  instructions)

(defun choose-next-available (instructions)
  "Returns the next key from instructions which is available, i.e. which has no
dependencies."
  (iter
    (for (key value) in-hashtable instructions)
    (unless (gethash key instructions)
      (collect key into result))
    ;; The order should not matter in topology but here we will sort the keys
    ;; alphabetically.
    (finally (return (first (sort result #'string<))))))

(defun remove-step (step instructions)
  "Returns the hash INSTRUCTIONS with the last STEP removed."
  (iter
    (for (key value) in-hashtable instructions)
    (setf (gethash key instructions) (remove step value :test #'equal))
    (remhash step instructions))
  instructions)

(defun follow-instructions (steps)
  "Returns a string with the right order of the instructions."
  (iter
    (for instructions initially (instructions steps)
         then (remove-step next instructions))
    (for next = (choose-next-available instructions))
    (dbg :07-fi ";;; ~A" next)
    (while next)
    (collect next into ordered-instructions)
    (finally (return (map 'string (lambda (s) (char s 0 )) ordered-instructions)))))

(defun aoc18-07a (&optional (input #p"inputs/input7.txt"))
  "The first part of day 7."
  (-> input
      (read-input7)
      (follow-instructions)))

;;; Part 2
(defstruct worker
  "A structure holding the current information for each worker."
  current-task time-to-go)

(defun time-per-step (step &optional (duration 60))
  "Returns the amount of time a STEP takes."
  (+ duration (- (char-code (char step 0)) 64)))

(defun no-one-working-p (workers task)
  "Returns T if no worker is working on the next task."
  ;;; I am using strings as keys of the hash so we have to use equal here.
  (not (some (lambda (x) (equal task
			 (worker-current-task x)))
	     workers)))

(defun next-available (instructions workers)
  "Returns the next available step depending on the workers state."
  (find-if (lambda (x) (no-one-working-p workers x))
	   (iter
             (for (key value) in-hashtable instructions)
             (unless (gethash key instructions)
               (collect key into result))
             ;; The order should not matter in topology but here we will sort the keys
             ;; alphabetically.
             (finally (return (sort result #'string<))))))

(defun follow-instructions-with-workers (steps
                                         &aux (ordered-instructions
                                               (make-array 0 :element-type 'character
                                                             :adjustable t :fill-pointer 0)))
  "Returns a string with the right order of the instructions and the duration in seconds."
  (iter
    (with workers = (make-array 5 :element-type 'worker
                                  :initial-contents (iter (repeat 5)
                                                      (collect (make-worker :time-to-go 0)))))
    (with instructions = (instructions steps))
    (for second from 0)
    ;;; We will iterate over all seconds and in a inner loop over the workers
    (dbg :07b ";;; Second: ~D~%" second)
    (iter
      (for w in-vector workers)
      (dbg :07b-inner ";;;; Worker: ~A time: ~D~%" w (worker-time-to-go w))
      (cond ((zerop (worker-time-to-go w)) ; The worker is idle
             (when (worker-current-task w) ; But had a task so this task is finished
	       (setf
                instructions (remove-step (worker-current-task w) instructions)
                (worker-current-task w) nil)) ; No the nicest code but it worked
	     (let ((next-letter (next-available instructions workers))) ; Finding the next free task
               (dbg :07b-inner ";;;; Next: ~A~%" next-letter)
	       (when next-letter
		 (setf (worker-current-task w) next-letter
		       (worker-time-to-go w)
		       (1- (time-per-step next-letter))) ; 1- beause one second is already gone.
                 (vector-push-extend (char next-letter 0) ordered-instructions)))) ; Saving the order
            (t (decf (worker-time-to-go w))))) ; Reducing the time to go for the current worker.
    (until (every (lambda (x) (null (worker-current-task x)))
		  workers))
    (finally (return (values ordered-instructions second)))))

(defun aoc18-07b (&optional (input "inputs/input7.txt"))
  (-> input
      (read-input7)
      (follow-instructions-with-workers)))

(define-test test-07
  (assert-equal 61 (time-per-step "A"))
  (assert-equal 86 (time-per-step "Z")))




