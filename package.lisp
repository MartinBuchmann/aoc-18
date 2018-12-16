;;;; package.lisp

(defpackage :aoc-18
  (:use :cl :iterate :lisp-unit)
  (:import-from :anaphora
   :awhen :it)
  (:import-from :alexandria
   :curry)
  (:import-from :cl-ppcre
   :split))

