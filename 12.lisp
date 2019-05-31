;; -*- ispell-local-dictionary: "en" -*-
;; * Day 12
;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; I knew that a simple cellular automaton would yield most likely to
;; the solution. After studying Phil! Gold's solution I copied his
;; great idea using bit-vectors for that tasks.

;; * Some basic parameters

;; The initial state string starts with an offset for the prefix
;; "initial state: "
(defparameter *state-prefix-length* 15)

;; The encoding of plants and empty pots
(defparameter *plant* #\#)
(defparameter *empty* #\.)

;; * A structure for the state
;;
;; Because the plants can and will spread across the left and right
;; border of the initial state I will keep an offset for the extra
;; space.
(defstruct state
  (offset 0)
  pots)

;; * Handling the input

;; ** Convert the state-string to a bit-vector
(defun parse-state-string (state)
  "Parses the STATE string and returns a bit vector where each cell
with a plant is represented by 1."
  (iter (with bit-vector = (make-array (length state)
                                       :element-type 'bit
                                       :initial-element 0))
        (for c in-string state with-index i)
        (when (char= c *plant*)
          (setf (sbit bit-vector i) 1))
        (finally (return bit-vector))))

;; ** Reading the initial state                               
(defun parse-initial-state (initial-state-string)
  "Return a STATE structure for the INITIAL-STATE-STRING."
  (let* ((bit-string (subseq initial-state-string *state-prefix-length*))
         (first-plant (position *plant* bit-string))
         (last-plant (position *plant* bit-string :from-end t)))
    (make-state
     :offset (min 0 (- first-plant 4))
     :pots (parse-state-string
            (format nil "~{~A~}~A~{~A~}"
                    (make-list (max 0 (- 4 first-plant))
                               :initial-element *empty*)
                    bit-string
                    (make-list (max 0 (- last-plant (length bit-string) -5))
                               :initial-element *empty*))))))

;; ** Converting the pattern to an integer value
(defun pattern-to-int (pattern)
  "Converts the given PATTERN of the rule to an integer value."
  (if (zerop (length pattern))
      0
      (+ (ash (pattern-to-int (subseq pattern 0 (1- (length pattern))))
              1)
         (if (char= (schar pattern (1- (length pattern)))
                    *plant*)
             1
             0))))

;; ** Reading the rules
;;
;; Rules are 5 character string where each character can take two
;; state: plant or empty. This leads to 2^5 = 32 possible rules. A
;; 32bit vector can thus hold all rules. The value of the bit gives
;; the information if a plant will grow in the next generation or not. 
(defun parse-rules (rule-string-list)
  "Returns an 32bit vector for all possible rules."
  (iter (with result = (make-array (expt 2 5) :element-type 'bit :initial-element 0))
        (for rule-string in rule-string-list)
        (for (pattern-string outcome-string) = (ppcre:split " => " rule-string))
        (when (char= (schar outcome-string 0) *plant*)
          (setf (sbit result (pattern-to-int pattern-string)) 1))
        (finally (return result))))

;; ** Reading the data
;;
;; I found the use of destructing-bind very simple even though it is
;; not very elegant.
(defun read-input12 (&optional (input #p"inputs/input12.txt"))
  "Reads the input for AOC-18 day 10 and returns it as a list of LIGHTS."
  (destructuring-bind (initial-states empty &rest rules)
      (iter
        (for line in-file input using #'read-line)
        (collect line))
    @ignore empty
    (list initial-states rules)))

;; ** Storing the data
(defvar *test-input* (read-input12 "tests/tests-12.txt"))
(defvar *test-state* (parse-initial-state (nth 0 *test-input*)))
(defvar *test-rules* (parse-rules (nth 1 *test-input*)))

(defvar *input* (read-input12 "inputs/input12.txt"))
(defvar *state* (parse-initial-state (nth 0 *input*)))
(defvar *rules* (parse-rules (nth 1 *input*)))

;; * The spreading of the plants
(defun spread-plants-once (state rules)
  "Returns a new-state after applyint RULES to the given STATE."
  (with-slots ((current-offset offset) (current-pots pots)) state
    (let* ((first-plant (position 1 current-pots))
           (last-plant (position 1 current-pots :from-end t))
           (new-offset (- current-offset (- 6 first-plant)))
           (new-length (+ last-plant 7 (- current-offset new-offset)))
           (new-pots (make-array new-length :element-type 'bit :initial-element 0)))
      (iter (for bit in-vector current-pots
                 from first-plant to (+ last-plant 4)
                 with-index i)
            (for pattern first 1 then (logand (logior (ash pattern 1)
                                                      bit)
                                              #b11111))
            (when (plusp (sbit rules pattern))
              (setf (sbit new-pots (+ i -2 (- current-offset new-offset)))
                    1)))
      (make-state :offset new-offset :pots new-pots))))

;; * Spreading the plants for generations
(defun spread-plants (initial-state rules generations)
  "Return the state after GENERATTIONS for RULES and the INITIAL-STATE."
  (iter (for generation from 1 to generations)
        (for new-state first (spread-plants-once initial-state rules)
             then (spread-plants-once new-state rules))
        (for previous-state previous new-state) ; I really like iterate
        (when (and previous-state
                   (equal (state-pots new-state) (state-pots previous-state)))
          (leave (make-state
                  :offset (+ (state-offset new-state)
                             (* (- (state-offset new-state) (state-offset previous-state))
                                (- generations generation)))
                  :pots (state-pots new-state))))
        (finally (return new-state))))

;; * Adding the plant numbers
(defun add-plant-numbers (state)
  (with-slots (offset pots) state
    (iter (for p in-vector pots with-index i)
          (when (= p 1)
            (summing (+ i offset))))))

;; * Part 1
(defun aoc-12a ()
  (add-plant-numbers (spread-plants *state* *rules* 20)))

;; * Part 2
(defun aoc-12b ()
  (add-plant-numbers (spread-plants *state* *rules* 50000000000)))

(define-test test-12
  (assert-equalp '#*0000001000100001000001001001001000000
                 (slot-value (spread-plants-once *test-state* *test-rules*) 'pots))
  (assert-equal 325 (add-plant-numbers (spread-plants *test-state* *test-rules* 20)))
  (assert-equal 1787 (aoc-12a)))

