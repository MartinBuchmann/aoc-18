;;;; * Day 9
;;; The package
(in-package :aoc-18)

;; Using Phil! Golds clist implementation
;; https://gitlab.com/asciiphil/advent-of-code/blob/master/clist.lisp

;; ** The board
(defun init-board ()
  (clist:make-circular-list 0))

;; ** Reading the input
(defun read-input9 (&optional (input #p"inputs/input9.txt"))
  "Reads the input for AOC-18 day 9 and returns it as a list of
integers (number-of-players max-points)."
  (-<> (read-file-into-string input)
       (split "\\s" <>)
       (mapcar (lambda (s) (parse-integer s :junk-allowed t)) <>)
       (remove nil <>)))

;; Alternatively, one could use a RegEx
;; (ppcre:register-groups-bind ((#'parse-integer x y))
;;     ("\\D+(\\d+)\\D+(\\d+)\\D?" "abcd 403 abcd 123  e ")
;;   (list x y))

;; ** Part 1

(defun place-marble (board marble-number)
  "Returns two values: the state of the board after placing the marble;
  and the score received by the current player after their move."
  ;; Check if the marble number is a multiple of 23 
  (if (/= 0 (mod marble-number 23))
      (values (clist:insert (clist:rotate board 1)
                            marble-number
                            :right)
              0)
      ;; Remove the 7th marbel counter-clockwise and return the score
      (let ((new-board (clist:rotate board -7)))
        (values (clist:remove-focused new-board)
                (+ marble-number (clist:focused new-board))))))

(defun play-game (players last-marble)
  "Play the marble game of the Elves as defined in day 9 of AOC-18."
  (iter (with initial-board = (init-board))
        (with scores = (make-array players))
        (for marble from 1 to last-marble)
        (for player first 0
             then (mod (1+ player) players))
        (for (values board score)
             first (place-marble initial-board marble)
             then (place-marble board marble))
        (incf (svref scores player) score)
        (dbg :09-a "[~A:~A] ~A~%" player score board)
        (finally (return scores))))

(defun find-winning-score (players last-marble)
  (reduce #'max (play-game players last-marble)))

(defun aoc-09a ()
  "My solution of the first part of day 9."
  (-<> (read-input9)
       (destructuring-bind (players last-marble)
           <> (find-winning-score players last-marble))))

;; ** Part 2

(defun aoc-09b ()
  "My solution of the second part of day 9."
  (-<> (read-input9)
       (destructuring-bind (players last-marble)
           <> (find-winning-score players (* 100 last-marble)))))





;; ** Testing

(define-test test-09
  (assert-equal 32 (find-winning-score 9 25))
  (assert-equal 8317 (find-winning-score 10 1618))
  (assert-equal 146373 (find-winning-score 13 7999))
  (assert-equal 2764 (find-winning-score 17 1104))
  (assert-equal 54718 (find-winning-score 21 6111))
  (assert-equal 37305 (find-winning-score 30 5807)))
