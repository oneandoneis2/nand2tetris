#!/usr/bin/clisp

; Define the Parse class
(defclass parse ()
  ((filename :initarg :file
             :reader filename)
   (line :initform 0 :accessor line)
   (data :initform nil :accessor data)))

; Define the Code class
(defclass code ()
  ((dest :initarg :dest :reader d)
   (comp :initarg :comp :reader c)
   (jump :initarg :jump :reader j)))

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

(defun num->bin (num size)
  (let ((str ""))
    (loop while (> size 0)
          do (if (= 1 (mod num 2))
               (setf str (concatenate 'string "1" str))
               (setf str (concatenate 'string "0" str)))
          (setf num (ash num -1))
          (decf size))
    str))

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
      nil)))

(defmethod dest ((c code))
  (let ((cmd (d c)))
    (if cmd
      (num->bin (+ (if (search "M" cmd) 1 0)
                   (if (search "D" cmd) 2 0)
                   (if (search "A" cmd) 4 0))
                3)
      "000")))

(defmethod comp ((p parse))
  (let ((cmd (current p)))
    ; C-command format is foo=bar;baz
    ; but only 'bar' is gauranteed
    (let ((eqpos (+ 1 (or (search "=" cmd) -1)))
          (semipos (or (search ";" cmd) (length cmd))))
      (subseq cmd eqpos semipos))))

(defmethod comp ((c code))
  (gethash (c c) *mnemonics*))

(defmethod jump ((p parse))
  (let* ((cmd (current p))
         (semipos (search ";" cmd)))
    (if semipos
      (subseq cmd (+ 1 semipos) (length cmd))
      nil)))

(defmethod jump ((c code))
  (let ((cmd (j c)))
    (if cmd
      (num->bin (cond ((equal cmd "JGT") 1)
                      ((equal cmd "JEQ") 2)
                      ((equal cmd "JGE") 3)
                      ((equal cmd "JLT") 4)
                      ((equal cmd "JNE") 5)
                      ((equal cmd "JLE") 6)
                      ((equal cmd "JMP") 7))
                3)
      "000")))

; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))
(defvar *mnemonics* (make-hash-table :test 'equal))
(setf (gethash "0"   *mnemonics*) "0101010")
(setf (gethash "1"   *mnemonics*) "0111111")
(setf (gethash "-1"  *mnemonics*) "0111010")
(setf (gethash "D"   *mnemonics*) "0001100")
(setf (gethash "A"   *mnemonics*) "0110000")
(setf (gethash "!D"  *mnemonics*) "0001101")
(setf (gethash "!A"  *mnemonics*) "0110001")
(setf (gethash "-D"  *mnemonics*) "0001111")
(setf (gethash "-A"  *mnemonics*) "0110011")
(setf (gethash "D+1" *mnemonics*) "0011111")
(setf (gethash "A+1" *mnemonics*) "0110111")
(setf (gethash "D-1" *mnemonics*) "0001110")
(setf (gethash "A-1" *mnemonics*) "0110010")
(setf (gethash "D+A" *mnemonics*) "0000010")
(setf (gethash "D-A" *mnemonics*) "0010011")
(setf (gethash "A-D" *mnemonics*) "0000111")
(setf (gethash "D&A" *mnemonics*) "0000000")
(setf (gethash "D|A" *mnemonics*) "0010101")
(setf (gethash "M"   *mnemonics*) "1110000")
(setf (gethash "!M"  *mnemonics*) "1110001")
(setf (gethash "-M"  *mnemonics*) "1110011")
(setf (gethash "M+1" *mnemonics*) "1110111")
(setf (gethash "M-1" *mnemonics*) "1110010")
(setf (gethash "D+M" *mnemonics*) "1000010")
(setf (gethash "D-M" *mnemonics*) "1010011")
(setf (gethash "M-D" *mnemonics*) "1000111")
(setf (gethash "D&M" *mnemonics*) "1000000")
(setf (gethash "D|M" *mnemonics*) "1010101")

(defvar *command* nil)
(loop for line = (current *parse*)
      while (hasMoreCommands *parse*)
      do (progn
           (cond ((or (eq 'A (commandType *parse*))
                      (eq 'L (commandType *parse*)))
                  (format t "0~a~%" (num->bin (parse-integer (symbol *parse*)) 15)))
                 ((eq 'C (commandType *parse*))
                  (let ((code (make-instance 'code
                                             :dest (dest *parse*)
                                             :comp (comp *parse*)
                                             :jump (jump *parse*))))
                    (format t "111~a~a~a~%" (comp code) (dest code) (jump code)))))
           (advance *parse*)))
