(in-package :aoc-18)

(defun reactivep (a b)
  "Returns T if A and B or of the same type but different polarity,
  I.e. the same letter but uppercase and lowercase, respectively."
  (and (not (char= a b)) (char-equal a b)))

(defun eliminate-reactive-pairs (data)
  "Returns the resulting string after eliminating all reactive pairs
from DATA."
  (iter
    (with result = (make-array 0
                               :element-type 'character
                               :adjustable t
                               :fill-pointer 0))
    (with last-char = (char data (1- (length data))))
    (generate i from 0 below (1- (length data)))
    (next i)
    (for a = (char data i))
    (for b = (char data (1+ i)))
    (if (reactivep a b)
        (next i)
        (vector-push-extend a result))
    (finally 
     (unless (reactivep a last-char)
       (vector-push-extend last-char result))
     (return result))))

(defun shrink-string (data)
  (iter
    (for reduced-string initially (eliminate-reactive-pairs data)
         then (eliminate-reactive-pairs reduced-string))
    (for len = (length reduced-string))
    (for prev-len previous len)
    (until (and prev-len (= len prev-len)))
    (finally (return reduced-string))))

(defun aoc18-05a (&optional (input #p"inputs/input5.txt"))
  (-<> (string-trim '(#\Newline) (read-file-into-string input))
       (shrink-string <>)
       (length <>)))

(defun aoc18-05b (&optional (input #p"inputs/input5.txt"))
  (iter
    (with data = (string-trim '(#\Newline) (read-file-into-string input)))
    (for c in-string "abcdefghijklmnopqrstuvwxyz")
    (for tmp = (remove c data :test #'char-equal))
    (dbg :05b ";;; Candidate: ~A~%" tmp)
    (minimize (length (shrink-string tmp)) into min)
    (finally (return min))))

(define-test test-05
  (assert-true (reactivep #\a #\A))
  (assert-nil (reactivep #\a #\a))
  (assert-nil (reactivep #\a #\B))
  (assert-nil (reactivep #\a #\b))
  (assert-equal "dabAaCBAcaDA"
   (eliminate-reactive-pairs "dabAcCaCBAcCcaDA"))
  (assert-equal "dabAaCBAcaDA"
   (eliminate-reactive-pairs "dabAcCaCBAcCcaDA"))
  (assert-equal "dabAaCBAcaD"
                (eliminate-reactive-pairs "dabAcCaCBAcCcaD"))
  (assert-equal 9348 (aoc18-05a))
  (assert-equal 4996 (aoc18-05b)))
