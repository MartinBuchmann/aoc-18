;;; -*- ispell-local-dictionary: "en_GB" -*-
;;; * Day 15
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; * The data structures for the battle

(defparameter *elf-power* 3 "The power of an elf attack.")
(defparameter *neighbors* '(#C(0 -1) #C(-1 0) #C(1 0) #C(0 1)))

(defclass entity ()
  ((char :initarg :char
         :reader entity-char)
   (position :initarg :pos
             :reader entity-position))
  (:documentation "The objects of the world: Walls (#), Elves (E) and Goblins (G)."))

(defmethod print-object ((entity entity) stream)
  (with-slots (char) entity
    (format stream "#<ENTITY ~A>" char)))

(defclass wall (entity) ())

(defmethod print-object ((entity wall) stream)
  (with-slots (char) entity
    (format stream "#<WALL>")))

(defclass actor (entity)
  ((team :initarg :team
         :reader actor-team)
   (attack :initform 3
           :initarg :attack)
   (hp :initform 200
       :accessor actor-hp))
  (:documentation "The actors of the battle: Elves and Gobblins."))

(defmethod print-object ((entity actor) stream)
  (with-slots (char position hp) entity
    (format stream "#<ACTOR ~A(~A)=~A>" char position hp)))

(defun row-order< (complex-1 complex-2)
  "A helper predicate comparing complex numbers, used as 2d coordinates."
  (or (< (imagpart complex-1) (imagpart complex-2))
      (and (= (imagpart complex-1) (imagpart complex-2))
           (< (realpart complex-1) (realpart complex-2)))))

(defun entity< (entity-1 entity-2)
  "Returns T if ENTITY-1 comes before ENTITY-2 on the map."
  (row-order< (slot-value entity-1 'position)
              (slot-value entity-2 'position)))

(defun entity-from-char (char position elf-power)
  "Returns a new object depending on CHAR."
  (ecase char
    (#\# (make-instance 'wall :pos position :char #.(code-char #x2593)))
    (#\G (make-instance 'actor :pos position :char #\G :team :goblin))
    (#\E (make-instance 'actor :pos position :char #\E :team :elf :attack elf-power))
    (#\. nil)))

(defstruct battle
  "The current state the battle has. MAP is a 2d array. Actors is a
  list of elves and goblins alive." map actors)

(defmethod print-object ((battle battle) stream)
  (with-slots (map) battle
    (format stream "#<BATTLE~%")
    (iter (for y from 0 below (array-dimension map 0))
          (format stream "  ")
          (for (entities actors) =
               (iter (for x from 0 below (array-dimension map 1))
                     (for e = (aref map y x))
                     (collecting e into entities)
                     (when (typep e 'actor)
                       (collecting e into actors))
                     (finally (return (list entities actors)))))
          (format stream "  ~{~A~}  ~:{~A(~A)~:^,~}~%"
                  (mapcar (lambda (e) (if e (entity-char e) " ")) entities)
                  (mapcar (lambda (a) (list (entity-char a) (actor-hp a))) actors)))
    (format stream "  >")))

(defun parse-battle (string-list &optional (elf-power *elf-power*))
  "Parses the given battle and returns a corresponding structure."
  (let ((map (make-array (list (length string-list) (length (first string-list)))
                         :element-type '(or null entity)
                         :initial-element nil)))
    (iter outer
          (for string in string-list)
          (for y from 0)
          (iter (for char in-string string with-index x)
                (for entity = (entity-from-char char (complex x y) elf-power))
                (setf (aref map y x) entity)
                (when (typep entity 'actor)
                  (in outer (collecting entity into actors))))
          (finally (return-from outer (make-battle :map map :actors actors))))))

(defun other-team (team)
  "Returns the other team."
  (if (eq team :elf)
      :goblin
      :elf))

(define-condition one-team-eliminated ()
  () (:documentation "Raise a condition if one team is completely eliminated."))

;; * The test input

(defparameter *test-basic-layout*
  '("#######"
    "#.G.E.#"
    "#E.G.E#"
    "#.G.E.#"
    "#######"))

(defparameter *test-movement-1*
  '("#######"
    "#E..G.#"
    "#...#.#"
    "#.G.#G#"
    "#######"))

(defparameter *test-movement-2*
  '("#######"
    "#.E...#"
    "#.....#"
    "#...G.#"
    "#######"))

(defparameter *test-movement-3*
  '("#########"
    "#G..G..G#"
    "#.......#"
    "#.......#"
    "#G..E..G#"
    "#.......#"
    "#.......#"
    "#G..G..G#"
    "#########"))

(defparameter *test-combat*
  '("#######"
    "#.G...#"
    "#...EG#"
    "#.#.#G#"
    "#..G#E#"
    "#.....#"
    "#######"))

(defparameter *test-combat-36334*
  '("#######"
    "#G..#E#"
    "#E#E.E#"
    "#G.##.#"
    "#...#E#"
    "#...E.#"
    "#######"))

(defparameter *test-combat-39514*
  '("#######"
    "#E..EG#"
    "#.#G.E#"
    "#E.##E#"
    "#G..#.#"
    "#..E#.#"
    "#######"))

(defparameter *test-combat-27755*
  '("#######"
    "#E.G#.#"
    "#.#G..#"
    "#G.#.G#"
    "#G..#.#"
    "#...E.#"
    "#######"))

(defparameter *test-combat-28944*
  '("#######"
    "#.E...#"
    "#.#..G#"
    "#.###.#"
    "#E#G#G#"
    "#...#G#"
    "#######"))

(defparameter *test-combat-18740*
  '("#########"
    "#G......#"
    "#.E.#...#"
    "#..##..G#"
    "#...##..#"
    "#...#...#"
    "#.G...G.#"
    "#.....G.#"
    "#########"))

;; * My input

(defun read-battle (&optional (input #p"inputs/input15.txt"))
  "Returns the given battle as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))

;; * Visualisation
;;
;; I couldn't stand the temptations to use some visualisation for the battle.  I
;; wanted to play with cl-cairo2 to draw the maps of each round of the battle as
;; a png file.  As I copied a lot of code from Phil I used also the dufy library
;; for defining the colours.

;; ** Parameters for visualisation

(defparameter *visualize* nil "Only output the visualisation if set to T. Use
with care as a lot of files will be created.")
(defparameter *square-side* 32 "The size of a square of the map in pixel.")

;; ** The colors (taken from Phil)

(defparameter *color-base03* (multiple-value-list (dufy:lab-to-xyz 15 -12 -12)))
(defparameter *color-base02* (multiple-value-list (dufy:lab-to-xyz 20 -12 -12)))
(defparameter *color-base01* (multiple-value-list (dufy:lab-to-xyz 45 -07 -07)))
(defparameter *color-base00* (multiple-value-list (dufy:lab-to-xyz 50 -07 -07)))
(defparameter *color-base0*  (multiple-value-list (dufy:lab-to-xyz 60 -06 -03)))
(defparameter *color-base1*  (multiple-value-list (dufy:lab-to-xyz 65 -05 -02)))
(defparameter *color-base2*  (multiple-value-list (dufy:lab-to-xyz 92 -00  10)))
(defparameter *color-base3*  (multiple-value-list (dufy:lab-to-xyz 97  00  10)))
(defparameter *color-dark-background* *color-base03*)
(defparameter *color-dark-highlight*  *color-base02*)
(defparameter *color-dark-secondary*  *color-base01*)
(defparameter *color-dark-primary*    *color-base0*)
(defparameter *color-dark-emphasized* *color-base1*)
(defparameter *color-light-background* *color-base3*)
(defparameter *color-light-highlight*  *color-base2*)
(defparameter *color-light-secondary*  *color-base1*)
(defparameter *color-light-primary*    *color-base00*)
(defparameter *color-light-emphasized* *color-base01*)

(defparameter *color-yellow*  (multiple-value-list (dufy:lab-to-xyz 60  10  65)))
(defparameter *color-orange*  (multiple-value-list (dufy:lab-to-xyz 50  50  55)))
(defparameter *color-red*     (multiple-value-list (dufy:lab-to-xyz 50  64  45)))
(defparameter *color-magenta* (multiple-value-list (dufy:lab-to-xyz 50  65 -05)))
(defparameter *color-violet*  (multiple-value-list (dufy:lab-to-xyz 50  15 -45)))
(defparameter *color-blue*    (multiple-value-list (dufy:lab-to-xyz 55 -10 -45)))
(defparameter *color-cyan*    (multiple-value-list (dufy:lab-to-xyz 60 -35 -05)))
(defparameter *color-green*   (multiple-value-list (dufy:lab-to-xyz 60 -20  65)))

(defun color (keyword)
  (symbol-value (find-symbol (format nil "*COLOR-~A*" keyword) (find-package :aoc-18))))

(defun set-color (keyword)
  (multiple-value-call #'cairo:set-source-rgb (apply #'dufy:xyz-to-rgb (color keyword))))

;; ** A macro to generate a png file for each state
(defmacro with-png-for-battle ((battle filename) &body body)
  "Executes BODY to puts its result for BATTLE in the png given by FILENAME."
  (let ((map (gensym "MAP")))
    `(if *visualize*
         (with-slots ((,map map)) ,battle
           (cairo:with-png-file (,filename
                                 :rgb24
                                 (* *square-side* (array-dimension ,map 1))
                                 (* *square-side* (array-dimension ,map 0)))
             (cairo:translate (/ *square-side* 2) (/ *square-side* 2))
             (cairo:scale *square-side* *square-side*)
             ,@body))
         (progn
           ,@body))))

;; ** The drawing functions

(defun draw-walls (battle)
  "Draws the walls of BATTLE's map."
  (when *visualize*
    (with-slots (map) battle
      (set-color :dark-highlight)
      (cairo:paint)
      (iter (for y from 0 below (array-dimension map 0))
            (iter (for x from 0 below (array-dimension map 1))
                  (for entity = (aref map y x))
                  (for x-base = (* x *square-side*))
                  (for y-base = (* y *square-side*))
                  (when (typep entity 'wall)
                    (set-color :dark-background)
                    (cairo:rectangle (- x 0.5) (- y 0.5) 1 1)
                    (cairo:fill-path)))))))

(defun draw-actors (battle)
  "Draws the actors of the BATTLE. Goblins are violet diamonds, elves are green
squares. The hit points are drawn as a small red/white indicator above the
symbol."
  (when *visualize*
    (with-slots (actors) battle
      (iter (for actor in actors)
            (when (not (plusp (actor-hp actor)))
              (next-iteration))
            (for x = (realpart (entity-position actor)))
            (for y = (imagpart (entity-position actor)))
            (ecase (actor-team actor)
              (:goblin
               (set-color :violet)
               (cairo:move-to (- x (* 0.25 (sqrt 2))) y)
               (cairo:line-to x (+ y (* 0.25 (sqrt 2))))
               (cairo:line-to (+ x (* 0.25 (sqrt 2))) y)
               (cairo:line-to x (- y (* 0.25 (sqrt 2))))
               (cairo:close-path)
               (cairo:fill-path))
              (:elf
               (set-color :green)
               (cairo:rectangle (- x 0.25) (- y 0.25) 0.5 0.5)
               (cairo:fill-path)))
            (set-color :light-background)
            (cairo:rectangle (- x 1/4) (- y 15/32) 1/2 2/32)
            (cairo:fill-path)
            (set-color :red)
            (cairo:rectangle (- x 1/4) (- y 15/32) (* 1/2 (/ (actor-hp actor) 200)) 2/32)
            (cairo:fill-path)))))

(defun draw-line (start end color width)
  (when *visualize*
    (cairo:set-line-width (/ width *square-side*))
    (set-color color)
    (cairo:move-to (realpart start) (imagpart start))
    (cairo:line-to (realpart end) (imagpart end))
    (cairo:stroke)))

(defun draw-cell (position color)
  (when *visualize*
    (set-color color)
    (cairo:rectangle (- (realpart position) 0.5)
                     (- (imagpart position) 0.5)
                     1 1)
    (cairo:fill-path)))

;; * Part 1

(defun living-actors (battle)
  "Returns living and dead actors as its second value."
  (values-list
   (iter (for actor in (battle-actors battle))
         (if (plusp (actor-hp actor))
             (collecting actor into alive)
             (collecting actor into dead))
         (finally (return (list alive dead))))))

(defun teams-present (battle)
  "Returns all present teams in the current battle as a list."
  (remove-duplicates (mapcar #'actor-team (living-actors battle))))

(defun neighbor-coords (position)
  "Returns a list with the coordinates of the neighbor fields."
  (mapcar (lambda (v) (+ position v)) *neighbors*))

(defun neighbor-actors (position battle)
  "Returns a list of living actors at neighbor coordinates."
  (remove-if-not (lambda (e) (and (typep e 'actor) (plusp (actor-hp e))))
                 (mapcar (lambda (p) (aref (battle-map battle) (imagpart p) (realpart p)))
                         (neighbor-coords position))))

(defun empty-neighbors (position battle)
  "Returns a list of empty neighbor fields."
  (remove-if (lambda (p)
               (cref (battle-map battle) p))
             (neighbor-coords position)))

(defun opponent-adjacent-p (my-team position battle)
  "Returns T if an opponent is within the direct neighborhood."
  (member (other-team my-team) (neighbor-actors position battle) :key #'actor-team))

(defun weakest-adjacent-opponent (my-team position battle)
  "Returns the weakest opponent within moving distance."
  (first (sort (remove my-team (neighbor-actors position battle) :key #'actor-team)
               #'<
               :key #'actor-hp)))

(defun nearest-targets (actor battle)
  "Returns a list of possible targets."
  (with-slots (position team) actor
    (let ((positions-visited (make-hash-table)))
      (labels ((search-distance (candidates)
                 (iter (for candidate in candidates)
                       (draw-cell candidate :dark-secondary))
                 (cond
                   ((endp candidates) nil)
                   ((member-if (lambda (c) (opponent-adjacent-p team c battle)) candidates)
                    (remove-if-not (lambda (c) (opponent-adjacent-p team c battle)) candidates))
                   (t
                    (iter (for candidate in candidates)
                          (setf (gethash candidate positions-visited) t))
                    (search-distance
                     (remove-if (lambda (c) (gethash c positions-visited))
                                (remove-duplicates
                                 (mapcan (lambda (c) (empty-neighbors c battle))
                                         candidates))))))))
        (search-distance (empty-neighbors position battle))))))

(defun first-step-to-target (source target battle)
  "Returns the path to the next target."
  (values-list
   (iter (for first-step in (empty-neighbors source battle))
         (for (values path cost) =
              (shortest-path first-step
                             (lambda (position)
                               (mapcar (lambda (p) (list 1 p)) (empty-neighbors position battle)))
                             :end target
                             :heuristic (lambda (position)
                                          (+ (abs (realpart (- target position)))
                                             (abs (imagpart (- target position)))))))
         (when cost
           (finding (list first-step path) minimizing cost)))))

(defun actor-move! (actor battle)
  "Move the actors in the current battle. See the rules of the puzzle for
details."
  (with-slots (team position) actor
    (if (opponent-adjacent-p team position battle)
        (draw-actors battle)
        (let ((target (first (sort (nearest-targets actor battle) #'row-order<))))
          (if target
              (progn
                (draw-line position target :magenta 2)
                (let ((first-step (first-step-to-target position target battle)))
                  (if first-step
                      (progn
                        (draw-line position first-step :blue 4)
                        (draw-actors battle)
                        (setf (cref (battle-map battle) position) nil
                              position first-step
                              (cref (battle-map battle) position) actor)
                        (draw-actors battle))))
                (draw-actors battle)))))))

(defun actor-combat! (actor battle)
  "Calculate the attack in the current battle."
  (with-slots (team position attack) actor
    (let ((opponent (weakest-adjacent-opponent team position battle)))
      (when opponent
        (draw-line position (entity-position opponent) :orange 4)
        (decf (actor-hp opponent) attack)
        (when (<= (actor-hp opponent) 0)
          (setf (cref (battle-map battle) (entity-position opponent)) nil))))))

(defun advance-round! (battle round-count)
  "Play one round in the current battle."
  (with-slots (actors map) battle
    (iter (for actor in actors)
          (when (<= (actor-hp actor) 0)
            (next-iteration))
          (for actor-count from 0)
          (when (< (length (teams-present battle)) 2)
            (signal 'one-team-eliminated))
          (with-png-for-battle (battle (format nil "images/2018-15-~2,'0D-~2,'0D.png"
                                               round-count actor-count))
            (draw-walls battle)
            (draw-cell (entity-position actor) :light-highlight)
            (actor-move! actor battle)
            (actor-combat! actor battle)))
    (setf actors (sort (living-actors battle) #'entity<))))

(defun determine-outcome (input &optional (elf-attack *elf-power*))
  (let ((battle (parse-battle input elf-attack)))
    (with-slots (actors) battle
      (iter (for round-count from 0)
            (handler-case
                (advance-round! battle round-count)
              (one-team-eliminated ()
                (setf (battle-actors battle) (living-actors battle))
                (return
                  (values (* round-count
                             (reduce #'+ (mapcar #'actor-hp (living-actors battle))))
                          round-count
                          battle))))))))

(defun aoc-15a (&optional (input #p"inputs/input15.txt"))
  "Returns the answer for the first part of day 15."
  (-> input
      read-battle
      determine-outcome))

;; * Part 2

(defun aoc-15b (&optional (input #p"inputs/input15.txt"))
  "Returns the answer of the second part of day 15."
  (flet ((elf-count (state)
           (count :elf (battle-actors state) :key #'actor-team)))
    (iter (with initial-battle = (read-battle input))
          (for attack from 3)
          (for initial-state = (parse-battle initial-battle attack))
          (for (values outcome rounds final-state) =
               (determine-outcome initial-battle attack))
          (for initial-elves = (elf-count initial-state))
          (for final-elves = (elf-count final-state))
          (finding (values outcome attack rounds final-state)
                   such-that (= initial-elves final-elves)))))

;; * Tests
(define-test test-15
  (assert-equal 27730 (determine-outcome *test-combat*)))

