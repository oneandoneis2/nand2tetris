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
          (t (error "Unknown type for command: ~a" cmd)))))

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

(defmethod symbol ((p parse))
  (let ((cmd (current p)))
    (cond ((stringstart cmd "@") (subseq cmd 1))
          ((stringstart cmd "(") (subseq cmd 1 (- (length cmd) 2)))
          (t (error "Can't get symbol from ~a" cmd)))))

(defmethod dest ((p parse))
  (let* ((cmd (current p))
         (eqpos (search "=" cmd)))
    (if eqpos
      (subseq cmd 0 eqpos)
      "")))

(defmethod comp ((p parse))
  (let ((cmd (current p)))
    ; C-command format is foo=bar;baz
    ; but only 'bar' is gauranteed
    (let ((eqpos (+ 1 (or (search "=" cmd) -1)))
          (semipos (or (search ";" cmd) (length cmd))))
      (subseq cmd eqpos semipos))))

(defmethod jump ((p parse))
  (let* ((cmd (current p))
         (semipos (search ";" cmd)))
    (if semipos
      (subseq cmd (+ 1 semipos) (length cmd))
      "")))

; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))
(defvar *command* nil)
(loop for line = (current *parse*)
      while (hasMoreCommands *parse*)
      do (progn
           (cond ((or (eq 'A (commandType *parse*))
                      (eq 'L (commandType *parse*)))
                  (format t "~a - " (symbol *parse*)))
                 ((eq 'C (commandType *parse*))
                  (format t "~a/~a/~a- " (dest *parse*) (comp *parse*) (jump *parse*))))
           (format t "~a ~a~%" (commandType *parse*) line)
           (advance *parse*)))
