;;;; package.lisp

(defpackage :aoc-18
  (:use :cl :iterate :lisp-unit :cl-arrows)
  (:import-from :anaphora
   :awhen :it)
  (:import-from :alexandria
   :curry :read-file-into-string)
  (:import-from :cl-ppcre
   :split :register-groups-bind))

