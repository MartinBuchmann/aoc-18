(in-package :aoc-18)

(defvar *last-id* 0)
(defvar *hash-of-guards* (make-hash-table))

(defun split-lines (line)
  "Splits LINE in the timestamp and the event and returns them as a list of strings."
  (let* ((pos (position #\] line))
         (time-stamp (subseq line 0 (1+ pos)))
         (event (subseq line (+ 2 pos))))
    (list time-stamp event)))

(defun sort-lines (lines)
  "Sorts the LINES by the timestamp, i.e. the first string in the list."
  (sort lines #'string< :key #'car))

(defun get-id (event)
  "Get the ID of the Guard if found in the event and returns the number."
  (let* ((pos-# (position #\# event))
         (pos-space (position #\space event :start (or pos-# 0))))
    (when pos-# (parse-integer (subseq event (1+ pos-#) pos-space)))))

(defun get-date (time-stamp)
  "Returns the date from the time-stamp."
  (let* ((pos- (position #\- time-stamp))
         (pos-space (position #\space time-stamp)))
    (subseq time-stamp (1+ pos-) pos-space)))

(defun get-minutes (time-stamp)
  "Returns the minutes from the timestamp."
  (let* ((pos-colon (position #\: time-stamp))
         (pos-] (position #\] time-stamp)))
    (parse-integer (subseq time-stamp (1+ pos-colon) pos-]))))

(defun parse-line (line)
  "The actual work : Parse each line checking if a new guard starts his shift,
if he falls asleep or wakes up and fill the hash with the corresponding information."
  (let* ((id (get-id (second line)))
         (minutes (get-minutes (first line)))
         (sleep-p (search "asleep" (second line))))
    (cond (id ; A new guard starts its shift 
           (setf *last-id* id)
           (unless #3=(gethash id *hash-of-guards*) (setf #3# nil)))
          (sleep-p ; He falls asleep
           (setf #1=(gethash *last-id* *hash-of-guards*) (cons minutes #1#)))
          (t ; He wakes up again
           (setf #2=(gethash *last-id* *hash-of-guards*) (cons (1- minutes) #2#))))
    (dbg :04-pl ";;; ID ~D ~A~%" *last-id* (gethash *last-id* *hash-of-guards*))))

(defun fill-hash-of-guards (input)
  "Reads in the data and fills the hash."
  (iter (for line in (sort-lines
                      (iter
                        (for line in-file input :using #'read-line)
                        (collect (split-lines line)))))
    (parse-line line)))

(defun sorted-lines (&optional (input #p"inputs/input4.txt")
                     &aux (lines (sort-lines
                                  (iter
                                    (for line in-file input :using #'read-line)
                                    (collect (split-lines line))))))
  "For debugging: Prints the sorted data."
  (format t "~{~S~%~}" lines))

(defun sleepy-guard (&aux (id 0) (max-sleep 0))
  "Returns the id of the GUARD sleeping the most."
  (iter
    (for (key value) in-hashtable *hash-of-guards*)
    (for sleep = (iter
                   (for (a b) on value by #'cddr)
                   (sum (- a b))))
    (when (> sleep max-sleep)
      (setf id key max-sleep sleep))
    (dbg :04a ";;; Guard: ~D; ~A~%" key sleep))
  id)

(defun aoc18-04a (&optional (input #p"inputs/input4.txt")
                  &aux (result (make-array 60 :element-type 'unsigned-byte :initial-element 0)))
  "The solution of the first part of day 4 AOC 2018."
  ;; Reading in the data and parsing the used information
  (fill-hash-of-guards input)
  (iter (for (key value) in-hashtable *hash-of-guards*)
    (dbg :04a ";;; ID ~D: ~A~%" key value))
  ;; Finding the guard which sleeps most
  ;; and the minute in which he sleeps mostly
  (let* ((guard (sleepy-guard))
         (times (gethash guard *hash-of-guards*))
         (minute 0))
    (iter
      (for (stop start) on times)
      (while (and start stop))
      (iter
        (for a from start to stop)
        (incf (aref result a))))
    (iter (for i from 0 to 59)
      (when (> (aref result i) (aref result minute))
        (setf minute i)))
    (values guard minute)))

(defun aoc18-04b (&aux (max-frequency 0) (max-minute 0) (max-guard 0))
  ;; The hash should be filled already by aoc18-04a
  (iter
    (for (guard times) in-hashtable *hash-of-guards*)
    (for minutes = (make-array 60 :initial-element 0 :element-type 'unsigned-byte))
    (dbg :04b-1 ";;; ID ~D: ~A~%" guard times)
    ;; Calculating for each guard in which minute he sleeps most frequently.
    (iter
      (for (stop start) on times by #'cddr)
      (iter
        (for a from start to stop)
        (incf (aref minutes a))))
    (dbg :04b-2 ";;; ID ~D: ~A~%" guard minutes)
    (iter (for i from 0 to 59)
      (when (> (aref minutes i) max-frequency)
        (setf max-minute i
              max-frequency (aref minutes i)
              max-guard guard))))
  (values max-guard max-minute max-frequency))


