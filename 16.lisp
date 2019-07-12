;;; -*- ispell-local-dictionary: "en_GB" -*-
;; * Day 16
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; * The data structure for instruction testing
(defstruct instruction-test
  "The state of the registers BEFORE and AFTER INSTRUCTION was performed."
  before
  instruction
  after)

;;; * Extracting the input

(defun extract-ints (string)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "[-+]?\\d+" string
                                 :sharedp t)))

(defun read-16 (&optional (input #p"inputs/input16.txt"))
  "Returns the input of day 16 as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))

(defun parse-instruction-testing (input)
  "Returns a list of structures with the instruction tests."
  (iter (generating line in input)
        (for before-str = (next line))
        (while (not (string= before-str "")))
        (for instruction-str = (next line))
        (for after-str = (next line))
        (collecting (make-instruction-test
                     :before (make-array 4 :initial-contents (extract-ints before-str))
                     :instruction (extract-ints instruction-str)
                     :after (make-array 4 :initial-contents (extract-ints after-str))))
        (next line)))

(defun parse-program (input)
  "Parses the second part of the input file, the test program."
  (iter (with in-program = nil)
        (with consecutive-blanks = 0)
        (for line in input)
        (when in-program
          (collecting (extract-ints line)))
        (if (string= line "")
            (incf consecutive-blanks)
            (setf consecutive-blanks 0))
        (when (and (not in-program)
                   (= 3 consecutive-blanks))
          (setf in-program t))))

;; * Defining the opcodes, etc.

(defvar *registers* nil "The registers of the device.")
(defvar *instructions* nil "The instructions for the devices.")

(defmacro register (n)
  "A macro to access the Nth register."
  `(svref *registers* ,n))

;; * Transforming the instructions to common lisp functions and predicates
(defmacro instruction (name (a b) &body body)
  "A macro transforming the instruction to a common lisp expression."
  (with-gensyms ((c "C"))
    ;; If the instruction ignores the register B the common lisp
    ;; expression will have a declare statement which has to be
    ;; handled.
    (let* ((has-declare (and (listp (car body))
                             (eq 'declare (caar body))))
           (declare-form (if has-declare
                             (car body)
                             nil))
           (body-form (if has-declare
                          (cdr body)
                          body)))
      `(progn
         (declaim (inline ,name))
         ;; Is the instruction already know?
         (when (not (member (quote ,name) *instructions*))
           (push (quote ,name) *instructions*))
         (defun ,name (,a ,b ,c)
           (declare (type (integer 0) ,a ,b ,c)) 
           ,(when declare-form declare-form)
           (setf (register ,c)
                 (progn ,@body-form)))))))

(defmacro instruction-func (name function &key (immediate nil))
  "A macro to transform the instruction to a common lisp function."
  (with-gensyms ((a "A") (b "B"))
    `(instruction ,name (,a ,b)
                  (,function (register ,a)
                             ,(if immediate
                                  b
                                  `(register ,b))))))

(defmacro instruction-compare (name function &key (first-immediate nil) (second-immediate nil))
  "A macro to transform the instruction to a predicate."
  (with-gensyms ((a "A") (b "B"))
    `(instruction ,name (,a ,b)
                  (if (,function ,(if first-immediate
                                      a
                                      `(register ,a))
                                 ,(if second-immediate
                                      b
                                      `(register ,b)))
                      1
                      0))))

;; * Defining the instructions

(instruction-func addr +)
(instruction-func addi + :immediate t)

(instruction-func mulr *)
(instruction-func muli * :immediate t)

(instruction-func banr logand)
(instruction-func bani logand :immediate t)

(instruction-func borr logior)
(instruction-func bori logior :immediate t)

(instruction setr (a b)
             (declare (ignore b))
             (register a))
(instruction seti (a b)
             (declare (ignore b))
             a)

(instruction-compare gtir > :first-immediate t)
(instruction-compare gtri > :second-immediate t)
(instruction-compare gtrr >)

(instruction-compare eqir = :first-immediate t)
(instruction-compare eqri = :second-immediate t)
(instruction-compare eqrr =)

;; * The input and test parameters

(defparameter *sample-instruction-test*
  (first (parse-instruction-testing '("Before: [3, 2, 1, 1]"
                                      "9 2 1 2"
                                      "After:  [3, 2, 2, 1]"))))

(defparameter *instruction-tests* (parse-instruction-testing (read-16)))
(defparameter *program* (parse-program (read-16)))

;; * Part 1

(defun instruction-test-matches-p (mnemonic instruction-test)
  "Returns T if MNEMONIC fits the given INSTRUCTION-TEST."
  (with-slots (before instruction after) instruction-test
    (setf *registers* (copy-seq before)) ;; copy-seq seems to be necessary?!
    (apply mnemonic (cdr instruction))
    (equalp *registers* after)))

(defun find-matching-instructions (instruction-test)
  (iter (for instruction in *instructions*)
        (when (instruction-test-matches-p instruction instruction-test)
          (collecting instruction))))

(defun aoc-16a ()
  "Returns the answer for the first part of day 16."
  (iter (for instruction-test in *instruction-tests*)
        (counting (<= 3 (length (find-matching-instructions instruction-test))))))

;; * Part 2

(defparameter *opcode-map* (make-array (length *instructions*) :initial-element nil)
  "The map of the OPCODES and their numerical values.")

(defun fill-opcode-map ()
  "Fills the opcode map with possible opcodes from *INSTRUCTION-TESTS*"
  (iter (for instruction-test in *instruction-tests*)
        (for opcode = (first (instruction-test-instruction instruction-test)))
        (when (svref *opcode-map* opcode)
          (next-iteration))
        (for candidates = (remove-if (lambda (i) (position i *opcode-map*))
                                     (find-matching-instructions instruction-test)))
        (when (= 1 (length candidates))
          (setf (svref *opcode-map* opcode) (first candidates))))
  (when (position nil *opcode-map*)
    (fill-opcode-map)))

(defun run-program (program)
  (setf *registers* (make-array 4 :initial-element 0))
  (iter (for (i a b c) in program)
    (funcall (svref *opcode-map* i) a b c))
  *registers*)

(defun aoc-16b ()
  "Returns the answer of the second part of day 16."
  (fill-opcode-map)
  (svref (run-program *program*) 0))

;; * Tests
(define-test test-16
  (assert-true (instruction-test-matches-p #'mulr *sample-instruction-test*))
  (assert-true (instruction-test-matches-p #'addi *sample-instruction-test*))
  (assert-true (instruction-test-matches-p #'seti *sample-instruction-test*))
  (assert-equal 542 (aoc-16a))
  (assert-equal 575 (aoc-16b)))

