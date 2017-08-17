#!/usr/bin/clisp

; Define the Parse class
(defclass parse ()
  ((filename :initarg :file
             :reader filename)
   (line :initform 0 :accessor line)
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
  (> (- (size p) (line p)) 0))

(defmethod output ((p parse))
  (format t "~{~a~%~}" (data p)))

(defmethod size ((p parse))
  (length (data p)))

(defmethod advance ((p parse))
  (if (hasMoreCommands p)
    (incf (line p)))
  (nth (line p) (data p)))

; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))
(defvar *command* nil)
(loop while (hasMoreCommands *parse*)
      do (format t "~a~%" (advance *parse*)))
