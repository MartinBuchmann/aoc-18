;;; -*- ispell-local-dictionary: "en_GB" -*-
;; * Day 19
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; I am using the basic instructions defined in 16.lisp 

;; * Read in the program

;; Following Svante's solution which is highly acknowledge
;; https://github.com/Harleqin/advent-of-code-2018/blob/master/19.lisp

;; ** Parse a line of program code for the instruction
(defun parse-instruction (string)
  "Returns a list with CL code for the instruction."
  (register-groups-bind (((compose #'intern #'string-upcase) op)
                         (#'parse-integer a b c))
                        ("([a-z]{4})\\s+(\\d+)\\s+(\\d+)\\s+(\\d+)" string)
                        (list op a b c)))


(defun read-program-from-file (&optional (input #p"inputs/input19.txt"))
  "Reads in the program from INPUT."
  (with-open-file (in input)
    (read-program in)))

(defun read-program (stream)
  "Reads the program from STREAM and returns two values:

- The program as a vector
- The first value of the instruction register."
  (let ((ip-reg (parse-integer (read-line stream)
                               :start 4))
        ;; The first line is the instruction register
        ;; Then the program follows
        (program (iter
                   (for line = (read-line stream nil))
                   (while line)
                   (collect (parse-instruction line) :result-type 'vector))))
    ; (assert (not (find nil program)))
    (list program ip-reg)))

;; * Part 1

(defun aoc-19a (&key (input #p"inputs/input19.txt") (registers (make-array 6 :initial-element 0)))
  "Returns the answer for the first part of day 19."
  ;; Initialising the registers, not very lispy. Maybe I will re-write
  ;; the code in 16.lisp to use local variables some day.
  (setf *registers* registers)
  ;; Iterating through the program
  (iter
    ;; Read in the program and the first instruction pointer register
    (with (program ip-reg) = (read-program-from-file input))
    ;; The first instruction pointer should be 0
    (with ip = 0)
    ;; Continue while inside the program
    (while (< ip (length program)))
    ;; Read in the next instruction
    (for instruction = (aref program ip))
    (setf (register ip-reg) ip)
    (dbg :19-a ";;; ~a ~d ~a" *registers* ip (aref program ip))
    (apply (first instruction) (rest instruction))
    ;; (dbg :19-a ";;; ~a" *registers*)
    ;; (break)
    (setf ip (register ip-reg))
    (incf ip)
    (finally (return (register 0)))))

;; * Part 2

;; ** The program and its direct translation

;; The instruction pointer (register 3, in my case) is increased after
;; each step. If an instruction adds n to the instruction point,
;; i.e.the pointer is increased to n+1 implicitly. That means n line
;; are skipped.

;; Using the debug statement one can see that the reference number is
;; calculated in the first run to 10551389. As far as I understand the
;; first part it is about factorisation of the reference number. This
;; can be done much faster :-)

#|
#ip 3
 0  addi 3 16 3 ; Go to line 17
 1  seti 1 2 5  ; Set r5 to 1
 2  seti 1 3 2  ; Set r2 to 1
 3  mulr 5 2 1  ; Set r1 to the product of r5 to r2, which is 1 after the restart
 4  eqrr 1 4 1  ; If r1 = r4 sets r1 to 1 else to 0
 5  addr 1 3 3  ; And now adds r1 to r3, i.e. if r1 = r4 skips the next line
 6  addi 3 1 3  ; Adds 1 to r3, i.e. skips the next line
 7  addr 5 0 0  ; Adds r5 to r0
 8  addi 2 1 2  ; Increases r2 by 1
 9  gtrr 2 4 1  ; If r2 > r4 sets r1 to 1 else to 0
10  addr 3 1 3  ; And now adds r1 to r3, i.e. if r2 > r4 skips the next line
11  seti 2 5 3  ; Go to 3
12  addi 5 1 5  ; Increase r5 by 1
13  gtrr 5 4 1  ; If r5 > r4 set r1 to 1 else set it to 0
14  addr 1 3 3  ; Increase the instruction pointer by the value of r1
15  seti 1 2 3  ; Go to line 2 
16  mulr 3 3 3  ; The instruction pointer (* r3 r3) is outside the program --> exit
17  addi 4 2 4  ; Calculate the reference number
18  mulr 4 4 4
19  mulr 3 4 4
20  muli 4 11 4
21  addi 1 6 1
22  mulr 1 3 1
23  addi 1 21 1
24  addr 4 1 4
25  addr 3 0 3 ; For the first part r0 is 0, for the second it is 1 --> Skip the next line
26  seti 0 3 3 ; Go to the first line for the first part, in the second this line is skipped.
27  setr 3 4 1
28  mulr 1 3 1
29  addr 3 1 1
30  mulr 3 1 1
31  muli 1 14 1
32  mulr 1 3 1
33  addr 4 1 4
34  seti 0 3 0 ; Set r0 to zero
35  seti 0 7 3 ; And start the program again

|#

(defun aoc-19b (&optional (reference 10551389))
  "Returns the answer for the second part of day 19. The reference number was
calculated using the direct calculation of the elfcode."
  (iter (for i from 1 to reference)
    (when (zerop (mod reference i))
      (summing i))))

(defun aoc-19b% ()
  "The simple but not feasible solution. Run time exceeds my patience ;-)"
  (aoc-19a :registers (make-array 6 :initial-contents '(1 0 0 0 0 0))))

;; * The direct translation

;; Honestly, not sure if this is completely correct. Because the
;; calculation takes too long I didn't use this directly. It was just
;; a skecth to understand what the program is actually doing.

;; The calculation of the reference number is correct!

#|
(let ((r0 1) (r1 0) (r2 0) (r3 0) (r4 0) (r5 0)) ; The registers, r3 is the instruction pointer
  (tagbody
     (format t "~a ~a ~a ~a ~a ~a~%" r0 r1 r2 r3 r4 r5)
     (go 17)                            ; -> addi 4 2 4
   1
     (setf r5 1)
   2 
     (setf r2 1)
   3
     (setf r1 (* r5 r2))
     (format t "~a ~a ~a ~a ~a ~a~%" r0 r1 r2 r3 r4 r5)
     (when (= r1 r4)
       (incf r0 r5))
     (incf r2)
     (format t "~a ~a ~a ~a ~a ~a~%" r0 r1 r2 r3 r4 r5)
     (when (<= r2 r4)
       (go 3))
     (incf r5)
     (format t "~a ~a ~a ~a ~a ~a~%" r0 r1 r2 r3 r4 r5)
     (when (<= r5 r4)
       (go 2))
     (go halt) 
   17
     ;; Calculate the reference number
     (incf r4 2)
     (setf r4 (* r4 r4))
     (setf r4 (* 19 r4))             ; The current instruction pointer
     (setf r4 (* r4 11))
     (incf r1 6)
     (setf r1 (* r1 22))             ; The current instruction pointer
     (incf r1 21)
     (incf r4 r1)
     (setf r1 27)                    ; The current instruction pointer
     (setf r1 (* r1 28))             ; The current instruction pointer
     (incf r1 29)                    ; The current instruction pointer
     (setf r1 (* r1 30))             ; The current instruction pointer
     (setf r1 (* r1 14)) 
     (setf r1 (* r1 32))             ; The current instruction pointer
     (incf r4 r1)
     ;; Set r0 to zero and start the program again
     (setf r0 0)
     (format t "~a ~a ~a ~a ~a ~a~%" r0 r1 r2 r3 r4 r5)
     (go 1)
   halt))
|#

;; * Tests

(define-test test-19
  (assert-equal 6 (aoc-19a :input #p"tests/test-19.txt"))
  (assert-equal 1056 (aoc-19a))
  (assert-equal 10915260 (aoc-19b)))
