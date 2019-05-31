;; -*- ispell-local-dictionary: "en" -*-
;; * Day 13
;; The package
(in-package :aoc-18)
(annot:enable-annot-syntax)

;; * Defining the cart and track
;;
;; I will use CLOS to model the track and the cart. 

;; ** The track objects
(defclass track ()
  ((segments :initarg :segments :accessor segments)
   (carts :initarg :carts :accessor carts)
   (wrecks :initarg :wrecks :initform nil :accessor wrecks))
  (:documentation "An object to model the track."))

(defun make-track (segments carts &optional wrecks)
  "Returns a new track object defined by SEGMENTS, CARTS and WRECKS."
  (make-instance 'track :segments segments :carts carts :wrecks wrecks))

;; (defstruct (track
;;             (:constructor make-track (segments carts &optional wrecks)))
;;   segments
;;   carts
;;   (wrecks nil))

;; *** The hierachy of track segments
(defclass track-segment () ())

(defclass straight-track-segment (track-segment) ())
(defclass vertical-track-segment (straight-track-segment) ())
(defclass horizontal-track-segment (straight-track-segment) ())

(defclass curve (track-segment) ())
(defclass forward-curve (curve) ())
(defclass bottom-right-curve (forward-curve) ())
(defclass top-left-curve (forward-curve) ())
(defclass reverse-curve (curve) ())
(defclass bottom-left-curve (reverse-curve) ())
(defclass top-right-curve (reverse-curve) ())

(defclass track-intersection (track-segment) ())

;; ** The methods to return a specific character per track segment
(defgeneric segment-char (segment)
  (:documentation "The different types of segments."))
(defmethod segment-char ((segment null))                     #\ )
(defmethod segment-char ((segment track-segment))            #\?)
(defmethod segment-char ((segment horizontal-track-segment)) #.(code-char #x2550))
(defmethod segment-char ((segment vertical-track-segment))   #.(code-char #x2551))
(defmethod segment-char ((segment forward-curve))            #\\)
(defmethod segment-char ((segment bottom-right-curve))       #.(code-char #x2554))
(defmethod segment-char ((segment top-left-curve))           #.(code-char #x255D))
(defmethod segment-char ((segment reverse-curve))            #\/)
(defmethod segment-char ((segment bottom-left-curve))        #.(code-char #x2557))
(defmethod segment-char ((segment top-right-curve))          #.(code-char #x255A))
(defmethod segment-char ((segment track-intersection))       #.(code-char #x256C))


;; ** The cart object
;;
;; I am using a circular list for the next-turn to implement the
;; repetion of left, straight and right.

(defclass cart ()
  ((position :initarg :position :accessor cart-pos)
   (direction :initarg :direction :accessor direction)
   (next-turn :initarg :next-turn :initform (clist:make-circular-list '((0 . -1) (1 . 0) (0 . 1)))
              :accessor next-turn))
  (:documentation "An object for the cart defined by POSITION (a cons
  ox X and Y), DIRECTION (a cons) and its NEXT-TURN (a circular-list)."))

(defun make-cart (position direction &optional next-turn)
  "Returns a new CART object defined by POSITION, DIRECTION and NEXT-TURN."
  (make-instance 'cart :position position :direction direction
                       :next-turn (or next-turn (clist:make-circular-list '((0 . -1) (1 . 0) (0 . 1))))))

;; *** Printing a cart.
(defmethod print-object ((cart cart) stream)
  (with-slots (position) cart
    (format stream "#<CART ~A ~A >" position (cart-char cart))))

;; *** A predicate for carts regarding their position.
(defun cart< (cart-1 cart-2)
  "Returns T if the position of CART1 is less than the postion of CART2."
  (with-slots ((position-1 position)) cart-1
    (with-slots ((position-2 position)) cart-2
      (or (< (rest position-1) (rest position-2))
          (and (= (rest position-1) (rest position-2))
               (< (first position-1) (first position-2)))))))

;; I didn't know about sharpsign dot before :-)
;; http://www.lispworks.com/documentation/HyperSpec/Body/02_dhf.htm
;; The characters for the carts are black triangles 
(defun cart-char (cart &aux (dir (direction cart)))
  "Returns a character for the cart depending on its direction."
  (cond ((equal dir '( 0 .  1)) #.(code-char #x25BC))
        ((equal dir '( 0 . -1)) #.(code-char #x25B2))
        ((equal dir '( 1 .  0)) #.(code-char #x25B6))
        ((equal dir '(-1 .  0)) #.(code-char #x25C0))
        (t (error "Wrong direction for cart: ~a" cart))))

;; *** The methods for moving the cart
(defgeneric move-cart (cart next-track-segment)
  (:documentation "Moving the cart. Will return a new cart object with the updated position."))

(defun new-position (pos dir)
  "A little helper for updating the position of a cart. Because I
  implemented the position as a cons I need an extra function."
  (cons (+ (first pos)  (first dir))
        (+ (rest pos) (rest dir))))

(defmethod move-cart (cart (next-track-segment straight-track-segment))
  (with-slots (position direction next-turn) cart
    (make-cart (new-position position direction)
               direction
               next-turn)))

(defmethod move-cart (cart (next-track-segment forward-curve))
  (with-slots (position direction next-turn) cart
    (make-cart (new-position position direction)
               (cons (- (rest direction)) (- (first direction)))
               next-turn)))

(defmethod move-cart (cart (next-track-segment reverse-curve))
  (with-slots (position direction next-turn) cart
    (make-cart (new-position position direction)
               (cons (rest direction) (first direction))
               next-turn)))

(defmethod move-cart (cart (next-track-segment track-intersection))
  (with-slots (position direction next-turn) cart
    (make-cart (new-position position direction)
               (cons (- (* (first direction) (first (clist:focused next-turn)))
                        (* (rest direction) (rest (clist:focused next-turn))))
                     (+ (* (first direction) (rest (clist:focused next-turn)))
                        (* (rest direction) (first (clist:focused next-turn)))))
               (clist:rotate next-turn 1))))

(defun map-track-segment (char)
  "Returns the segment type for a given character."
  (ecase char
    (#\| 'vertical-track-segment)
    (#\- 'horizontal-track-segment)
    (#\/ 'reverse-curve)
    (#\\ 'forward-curve)
    (#\+ 'track-intersection)
    (#\> 'horizontal-track-segment)
    (#\< 'horizontal-track-segment)
    (#\^ 'vertical-track-segment)
    (#\v 'vertical-track-segment)
    (#\V 'vertical-track-segment)
    (#\  nil)))

(defun map-cart-direction (char)
  "Returns the direction for a given character."
  (case char
    (#\> '( 1 .  0))
    (#\< '(-1 .  0))
    (#\v '( 0 .  1))
    (#\V '( 0 .  1))
    (#\^ '( 0 . -1))
    (otherwise nil)))


(defun track-curve-type (general-shape coordinates segments)
  "Determine the curve type for a given segment."
  (let ((x (first coordinates))
        (y (rest coordinates)))
    (if (eq general-shape 'forward)
        (cond
          ((and (plusp y)
                (member (type-of (aref segments (1- y) x))
                        '(vertical-track-segment track-intersection
                          bottom-right-curve bottom-left-curve)))
           'top-right-curve)
          ((and (plusp x)
                (member (type-of (aref segments y (1- x)))
                        '(horizontal-track-segment track-intersection
                          bottom-right-curve top-right-curve)))
           'bottom-left-curve)
          ((and (< y (array-dimension segments 0))
                (member (type-of (aref segments (1+ y) x))
                        '(vertical-track-segment track-intersection
                          top-right-curve top-left-curve)))
           'bottom-left-curve)
          ((and (< x (array-dimension segments 1))
                (member (type-of (aref segments y (1+ x)))
                        '(horizontal-track-segment track-intersection
                          bottom-left-curve top-left-curve)))
           'top-right-curve)
          (t
           (error "Couldn't determine ~A curve type for ~A." general-shape coordinates)))
        (cond
          ((and (plusp y)
                (member (type-of (aref segments (1- y) x))
                        '(vertical-track-segment track-intersection
                          bottom-right-curve bottom-left-curve)))
           'top-left-curve)
          ((and (plusp x)
                (member (type-of (aref segments y (1- x)))
                        '(horizontal-track-segment track-intersection
                          bottom-right-curve top-right-curve)))
           'top-left-curve)
          ((and (< y (array-dimension segments 0))
                (member (type-of (aref segments (1+ y) x))
                        '(vertical-track-segment track-intersection
                          top-right-curve top-left-curve)))
           'bottom-right-curve)
          ((and (< x (array-dimension segments 1))
                (member (type-of (aref segments y (1+ x)))
                        '(horizontal-track-segment track-intersection
                          bottom-left-curve top-left-curve)))
           'bottom-right-curve)
          (t
           (error "Couldn't determine ~A curve type for ~A." general-shape coordinates))))))

(defun parse-track (string-list)
  "Returns a track defined by the given STRING-LIST."
  (let ((track-segments
          (make-array (list (length string-list) (length (first string-list)))
                      :element-type '(or null track-segment)
                      :initial-element nil)))
    (iter outer
          (for string in string-list)
          (for y from 0)
          (iter (for c in-string string)
                (for cp previous c)
                (for x from 0)
                (for segment-type = (map-track-segment c))
                (when segment-type
                  (case segment-type
                    (forward-curve
                     (in outer (collecting (cons x y) into forward-curve-coordinates)))
                    (reverse-curve
                     (in outer(collecting (cons x y) into reverse-curve-coordinates)))
                    (otherwise
                     (setf (aref track-segments y x) (make-instance segment-type)))))
                (for cart-direction = (map-cart-direction c))
                (when cart-direction
                  (in outer
                      (collecting (make-cart (cons x y) cart-direction) into carts))))
          (finally
           (mapc (lambda (coords)
                   (setf (aref track-segments (rest coords) (first coords))
                         (make-instance (track-curve-type 'forward coords track-segments))))
                 forward-curve-coordinates)
           (mapc (lambda (coords)
                   (setf (aref track-segments (rest coords) (first coords))
                         (make-instance (track-curve-type 'reverse coords track-segments))))
                 reverse-curve-coordinates)
           (return-from outer (make-track
                               track-segments
                               (sort carts #'cart<)))))))

;; ** Printing a track
(defun print-track (track)
  "Printing a given TRACK."
  (with-slots (segments wrecks) track
    (labels ((print-rows (y carts)
               (when (< y (array-dimension segments 0))
                 (let ((remaining-carts (print-cells 0 y carts)))
                   (format t "~%")
                   (print-rows (1+ y) remaining-carts))))
             (print-cells (x y carts)
               (if (< x (array-dimension segments 1))
                   (cond
                     ((and (not (endp carts))
                           (equal (cons x y) (cart-pos (first carts))))
                      (format t "~A" (cart-char (first carts)))
                      (print-cells (1+ x) y (rest carts)))
                     ((member (cons x y) wrecks)
                      (format t "~A" #.(code-char #x2591))
                      (print-cells (1+ x) y carts))
                     (t
                      (format t "~A" (segment-char (aref segments y x)))
                      (print-cells (1+ x) y carts)))
                   carts)))
      (print-rows 0 (carts track)))))

;; * The test data
;;
(defparameter *test-track-1*
  (parse-track '("|"
                 "v"
                 "|"
                 "|"
                 "|"
                 "^"
                 "|"))
  "A straight track with two carts.")

(defparameter *test-track-2*
  (parse-track '("/->-\\        "
                 "|   |  /----\\"
                 "| /-+--+-\\  |"
                 "| | |  | v  |"
                 "\\-+-/  \\-+--/"
                 "  \\------/   "))
  "A more complex track.")

(defparameter *test-track-3*
  (parse-track '("/>-<\\  "
                 "|   |  "
                 "| /<+-\\"
                 "| | | v"
                 "\\>+</ |"
                 "  |   ^"
                 "  \\<->/"))
  "Another complex track.")

;; * The given data
(defun read-track (&optional (input #p"inputs/input13.txt"))
  "Returns the given track as a list of strings."
  (iter (for line in-file input using #'read-line)
        (when line
          (collect line))))

(defparameter *track* (parse-track (read-track)))

;; * Stepping through the track
(defun step-time (track)
  "Returns the updated track one step after TRACK."
  (let ((wrecks (wrecks track)))
    (labels ((move-carts (unmoved-carts moved-carts)
               (if (endp unmoved-carts)
                   (sort moved-carts #'cart<)
                   (let* ((cart (car unmoved-carts))
                          (new-pos (new-position (cart-pos cart) (direction cart)))
                          (segment (aref (segments track)
                                         (rest new-pos)
                                         (first new-pos)))
                          (moved-cart (move-cart cart segment)))
                     (cond
                       ((member (cart-pos moved-cart) moved-carts :test #'equal :key #'cart-pos)
                        (push (cart-pos moved-cart) wrecks)
                        (move-carts (cdr unmoved-carts)
                                    (remove (cart-pos moved-cart) moved-carts
                                            :test #'equal :key #'cart-pos)))
                       ((member (cart-pos moved-cart) unmoved-carts :test #'equal :key #'cart-pos)
                        (push (cart-pos moved-cart) wrecks)
                        (move-carts (remove (cart-pos moved-cart) (cdr unmoved-carts)
                                            :test #'equal :key #'cart-pos)
                                    moved-carts))
                       (t
                        (move-carts (cdr unmoved-carts) (cons moved-cart moved-carts))))))))
      (let ((new-carts (move-carts (carts track) nil)))
        (make-track (segments track) new-carts wrecks)))))

;; * Part 1
(defun run-until-crash (track &optional (iterations 0))
  "Runs TRACK until the first crash."
  (if (not (endp (wrecks track)))
      (values track iterations)
      (run-until-crash (step-time track) (1+ iterations))))

(defun first-crash-location (track)
  "Returns the postion of the first crash for TRACK."
  (first (wrecks (run-until-crash track))))

(defun aoc-13a ()
  "Returns the answer for the first part of day 13."
  (first-crash-location *track*))

;; * Part 2
(defun run-until-one-cart (track &optional (iterations 0))
  "Returns the track with only one cart left."
  (if (endp (cdr (carts track)))
      (values track iterations)
      (run-until-one-cart (step-time track) (1+ iterations))))

(defun last-cart-location (track)
  "Returns the postion of the last cart."
  (cart-pos (first (carts (run-until-one-cart track)))))

(defun aoc-13b ()
  "Returns the answer for the second part of day 13."
  (last-cart-location *track*))

;; * Tests
(define-test test-13
  (assert-equal '(7 . 3) (first-crash-location *test-track-2*))
  (assert-equal '(6 . 4) (last-cart-location *test-track-3*)))
