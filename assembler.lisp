#!/usr/bin/clisp

; Define the Parse package
(defpackage :parse
  (:use :cl)
  (:export :new))

(in-package :parse)

(defun new (filename)
  (with-open-file (src filename)
    (loop for line = (read-line src nil)
          while line
          collect line)))

; Package defs all done, do the assembling!
(in-package :common-lisp-user)

(defvar *source* (parse:new (first *args*)))

(princ *source*)
