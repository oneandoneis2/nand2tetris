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
        (strip (with-open-file (src (filename p))
                 (loop for line = (read-line src nil)
                       while line
                       collect line)))))

(defmethod hasMoreCommands ((p parse))
  (> (- (size p) (line p)) 0))

(defmethod output ((p parse))
  (format t "~{~a~%~}" (data p)))

(defmethod size ((p parse))
  (length (data p)))

(defmethod advance ((p parse))
  (if (hasMoreCommands p)
    (incf (line p)))
  (current p))

(defmethod current ((p parse))
  (nth (line p) (data p)))

(defmethod commandType ((p parse))
  (let ((cmd (current p)))
    (cond ((stringstart cmd "@") 'A)
          ((stringstart cmd "(") 'L)
          ((search "=" cmd) 'C)
          ((search ";" cmd) 'C)
          (t 'other))))

(defun stringstart (str chr)
  (string= chr str :start2 0 :end2 1))

(defun strip (lst)
  (labels ((emptyp (str) (string= "" str))
           (purgecomment (str) (subseq str 0 (search "//" str)))
           (purgewhite (str) (string-trim '(#\Space #\Tab #\Newline) str)))
    (remove-if #'emptyp
               (mapcar
                 #'purgewhite
                 (mapcar #'purgecomment lst)))))


; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))
(defvar *command* nil)
(loop for line = (current *parse*)
      while (hasMoreCommands *parse*)
      do (progn
           (format t "~a ~a~%" (commandType *parse*) line)
           (advance *parse*)))
