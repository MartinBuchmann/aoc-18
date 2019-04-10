;; -*- ispell-local-dictionary: "en" -*-
;;;; * Day 10
;;; The package
(in-package :aoc-18)

;; ** New data structures

(defstruct lights
  "Each light has a POSITION (X Y) in the sky and a VELOCITY (∆x ∆y)."
  position
  velocity)

(defstruct bbox
  "The bounding box of a group of lights in the minimal area containing them all."
  left
  bottom
  top
  right)

;; ** Calculating the bounding box of the current sky.

(defun find-bbox (points)
  "Returning the bounding box of the group of LIGHTS given as a list of cons."
  (iter (for point in points)
        (minimizing (car point) into left)
        (minimizing (cdr point) into bottom)
        (maximizing (car point) into right)
        (maximizing (cdr point) into top)
        (finally (return (make-bbox :left left :right right
                                    :top top :bottom bottom)))))

(defun bbox-area (bbox)
  "Returns the area of BBOX."
  (* (- (bbox-right bbox) (bbox-left bbox))
     (- (bbox-top bbox) (bbox-bottom bbox))))

;; ** Reading the input

(defun parse-input10 (input)
  "Parses the INPUT as a list of strings and returns a list of LIGHTS."
  (mapcar (lambda (i)
            (register-groups-bind ((#'parse-integer x y dx dy))
                ("([-+ ]?\\d+),\\s+([-+ ]?\\d+)> velocity=<([-+ ]?\\d+),\\s+([-+ ]?\\d+)" i)
              (make-lights :position (cons x y)
                           :velocity (cons dx dy))))
          input))

(defun read-input10 (&optional (input #p"inputs/input10.txt"))
  "Reads the input for AOC-18 day 10 and returns it as a list of LIGHTS."
  (parse-input10
   (iter
     (for line in-file input using #'read-line)
     (collect line))))

;; ** Test data

(defparameter *10-test-data*
  '("position=< 9,  1> velocity=< 0,  2>"
    "position=< 7,  0> velocity=<-1,  0>"
    "position=< 3, -2> velocity=<-1,  1>"
    "position=< 6, 10> velocity=<-2, -1>"
    "position=< 2, -4> velocity=< 2,  2>"
    "position=<-6, 10> velocity=< 2, -2>"
    "position=< 1,  8> velocity=< 1, -1>"
    "position=< 1,  7> velocity=< 1,  0>"
    "position=<-3, 11> velocity=< 1, -2>"
    "position=< 7,  6> velocity=<-1, -1>"
    "position=<-2,  3> velocity=< 1,  0>"
    "position=<-4,  3> velocity=< 2,  0>"
    "position=<10, -3> velocity=<-1,  1>"
    "position=< 5, 11> velocity=< 1, -2>"
    "position=< 4,  7> velocity=< 0, -1>"
    "position=< 8, -2> velocity=< 0,  1>"
    "position=<15,  0> velocity=<-2,  0>"
    "position=< 1,  6> velocity=< 1,  0>"
    "position=< 8,  9> velocity=< 0, -1>"
    "position=< 3,  3> velocity=<-1,  1>"
    "position=< 0,  5> velocity=< 0, -1>"
    "position=<-2,  2> velocity=< 2,  0>"
    "position=< 5, -2> velocity=< 1,  2>"
    "position=< 1,  4> velocity=< 2,  1>"
    "position=<-2,  7> velocity=< 2, -2>"
    "position=< 3,  6> velocity=<-1, -1>"
    "position=< 5,  0> velocity=< 1,  0>"
    "position=<-6,  0> velocity=< 2,  0>"
    "position=< 5,  9> velocity=< 1, -2>"
    "position=<14,  7> velocity=<-2,  0>"
    "position=<-3,  6> velocity=< 2, -1>"))

(defparameter *10-test-lights* (parse-input10 *10-test-data*))
(defparameter *10-lights* (read-input10))

;; ** Part 1
;;
;; We need to determine when the points align. I will do this by solving the
;; linear equation of each pair of lights.

(defun intersection-time (l1 l2)
  "Returns the time value when the paths of the lights L1 and L2 intersect."
  (unless (or (zerop (car (lights-velocity l1)))
              (zerop (car (lights-velocity l2))))
    ;; Only if the lights are moving...
    ;; The components of the linear equations
    ;; y = m*x + b
    (let ((m1 (/ (cdr (lights-velocity l1))
                 (car (lights-velocity l1))))
          (x1 (car (lights-position l1)))
          (y1 (cdr (lights-position l1)))
          (m2 (/ (cdr (lights-velocity l2))
                 (car (lights-velocity l2))))
          (x2 (car (lights-position l2)))
          (y2 (cdr (lights-position l2))))
      (unless (= m1 m2)
        (let* ((x-intersection (/ (+ (* m1 x1)
                                     (- (* m2 x2))
                                     y2
                                     (- y1))
                                  (- m1 m2))))
          ;; Returning the time value
          (/ (- x-intersection x1)
             (car (lights-velocity l1))))))))

;; Iterating over all lights...
(defun intersection-times (lights)
  "Returns a list of all intersection times of LIGHTS."
  (iter outer
        (for points on lights)
        (for l1 = (first points))
        (iter (for l2 in (rest points))
              (in outer
                  (collecting (intersection-time l1 l2))))))

;; Median of all intersection times...
(defun most-likely-intersection (lights)
  (let ((intersections (remove nil (intersection-times lights))))
    (round (nth (floor (length intersections) 2)
                (sort (copy-list intersections) #'<)))))

;; How does the sky looks like at t=seconds?
(defun sky-at-time (lights seconds)
  "Returns a list of positions of LIGHTS after SECONDS moving time."
  (iter (for point in lights)
        ;; Adding the x and y components of the current point
        (collecting (cons (+ (car (lights-position point))
                             (* seconds (car (lights-velocity point))))
                          (+ (cdr (lights-position point))
                             (* seconds (cdr (lights-velocity point))))))))

;; Plotting the sky, i.e. lights t a given time.  
(defun plot-sky (sky)
  "A simple plotting function for the x and y components of the lights in SKY."
  (let ((bbox (find-bbox sky)))
    (iter (for y from (1- (bbox-bottom bbox)) to (1+ (bbox-top bbox)))
          (iter (for x from (1- (bbox-left bbox)) to (1+ (bbox-right bbox)))
                (format t (if (member (cons x y) sky :test #'equalp)
                              "#"
                              ".")))
          (format t "~%"))))

;; I like the idea of measure the clustering as used by Phil!

(defparameter *adjacency-threshold* (sqrt 2))

(defun find-adjacent-points (p points)
  "Finding all POINTS within the *adjacency-threshold* of P."
  (iter (for candidate in points)
        (when (<= (sqrt (+ (expt (- (car candidate) (car p)) 2)
                           (expt (- (cdr candidate) (cdr p)) 2)))
                  *adjacency-threshold*)
          (collecting candidate))))

(defun find-grouped-points (seed points)
  "Finding all points that are adjacent points of adjacent points of SEED."
  (let ((candidates (find-adjacent-points seed points)))
    (if (endp candidates)
        (values (list seed) points)
        (values-list
         (iter (for candidate in candidates)
               (for (values subgroup remaining)
                    first (find-grouped-points candidate (remove candidate points))
                    then (find-grouped-points candidate (remove candidate remaining)))
               (appending subgroup into result)
               (finally (return (list (cons seed result) remaining))))))))

(defun group-all-points (points)
  "Grouping the POINTS into adjacent points."
  (multiple-value-bind (group1 remaining)
      (find-grouped-points (car points) (cdr points))
    (if (endp remaining)
        (list group1)
        (cons group1 (group-all-points remaining)))))

(defun aligned-p (points)
  "Returns T if all POINTS can be grouped into groups for at least two points."
  (iter (for group in (group-all-points points))
        (always (< 1 (length group)))))


(defun find-alignment (lights)
  "Returns the sky and the time with the smallest bounding box."
  (values-list
   (iter 
     ;; Determine a good starting time for the search,
     (with start-time = (most-likely-intersection lights))
     ;; Stepping around the start-time
     (for delta first 0
          then (if (plusp delta)
                   (- delta)
                   (1+ (- delta))))
     (for time = (+ start-time delta))
     (for sky = (sky-at-time lights time))
     (counting (not (aligned-p sky)) into unaligned-count)
     ;; Somewhat arbitrary limit, but it gives us three unaligned constellations
     ;; on each side of the aligned area.
     (while (< unaligned-count 6))
     ;; Finding the smallest bounding box.
     (finding (list sky time) minimizing (bbox-area (find-bbox sky))))))

(defun aoc-10a ()
  "My solution of the first part of day 10."
  (-> (read-input10)
      (find-alignment)
      (plot-sky)))

;; ** Part 2

(defun aoc-10b ()
  "My solution of the second part of day 10."
  (nth-value 1 (find-alignment *10-lights*)))

;; ** Testing

(define-test test-10
  (assert-equal 3 (nth-value 1 (find-alignment (parse-input10 *10-test-data*))))
  (assert-equal 10521 (nth-value 1 (find-alignment *10-lights*))))
