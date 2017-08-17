#!/usr/bin/clisp

; Define the Parse class
(defclass parse ()
  ((filename :initarg :file
             :reader filename)
   (data :initform nil :accessor data)))

; Define the Parse methods
; On initialization, read the file
(defmethod initialize-instance :after ((p parse) &key)
  (setf (data p)
        (with-open-file (src (filename p))
          (loop for line = (read-line src nil)
                while line
                collect line))))

(defmethod hasMoreCommands ((p parse))
  (> (length (data p)) 0))

(defmethod output ((p parse))
  (format t "~{~a~%~}" (data p)))

; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))
(output *parse*)
