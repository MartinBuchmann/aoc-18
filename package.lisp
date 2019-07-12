(in-package :cl)

;;;; package.lisp

(defpackage :circular-list
  (:nicknames :clist)
  (:use :common-lisp
   :iterate)
  (:shadow :equal)
  (:export :make-circular-list
   :equal :focused
   :insert :rotate
   :remove-focused))

(defpackage :aoc-18
  (:use :cl :iterate :lisp-unit :cl-arrows)
  (:import-from :anaphora
   :awhen :aif :it)
  (:import-from :alexandria
   :curry :read-file-into-string :with-gensyms)
  (:import-from :cl-ppcre
   :split :register-groups-bind))



