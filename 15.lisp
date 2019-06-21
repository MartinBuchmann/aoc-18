;;; -*- ispell-local-dictionary: "en_GB" -*-
;;; * Day 15
;;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;;; * Helper functions
(defun cref (array complex-number)
  "Returns the value of the 2d ARRAY at the position given as a COMPLEX NUMBER."
  (aref array (imagpart complex-number) (realpart complex-number)))

(defun (setf cref) (new-value array complex-number)
  "Sets the value of the 2d ARRAY at the position given as a COMPLEX NUMBER to NEW-VALUE."
  (setf (aref array (imagpart complex-number) (realpart complex-number)) new-value))

;;; ** Shortest Path
;;; Taken from Phil! Gold. I do not understand it completely.

(defun find-shortest-path (finishedp next-options visited options test)
  (if (zerop (cl-containers:size options))
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

;;; * The data structures for the battle

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

;;; * The test input

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

;;; * My input

(defun read-battle (&optional (input #p"inputs/input15.txt"))
  "Returns the given battle as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))

;;; * Part 1

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
  (with-slots (team position) actor
    (unless (opponent-adjacent-p team position battle)
      (let ((target (first (sort (nearest-targets actor battle) #'row-order<))))
        (if target
            (progn
              ;; (draw-line position target :magenta 2)
              (let ((first-step (first-step-to-target position target battle)))
                (if first-step
                    (progn
                      (setf (cref (battle-map battle) position) nil)
                      (setf position first-step)
                      (setf (cref (battle-map battle) position) actor))))))))))

(defun actor-combat! (actor battle)
  (with-slots (team position attack) actor
    (let ((opponent (weakest-adjacent-opponent team position battle)))
      (when opponent
        (decf (actor-hp opponent) attack)
        (when (<= (actor-hp opponent) 0)
          (setf (cref (battle-map battle) (entity-position opponent)) nil))))))

(defun advance-round! (battle round-count)
  @ignore round-count ; used later
  (with-slots (actors map) battle
    (iter (for actor in actors)
          (when (<= (actor-hp actor) 0)
            (next-iteration))
          (when (< (length (teams-present battle)) 2)
            (signal 'one-team-eliminated))
          (actor-move! actor battle)
          (actor-combat! actor battle))
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

;;; * Part 2

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

;;; * Tests
(define-test test-15
  (assert-equal 27730 (determine-outcome *test-combat*)))

