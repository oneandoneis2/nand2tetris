#!/usr/bin/clisp

; Define the Parse class
(defclass parse ()
  ((filename :initarg :file
             :reader filename)
   (line :initform 0 :accessor line)
   (data :initform nil :accessor data)
   ; Put a hash in the object for the jumpcodes
   (jumpcodes :initform (let ((hash (make-hash-table :test 'equal)))
                          (loop for (key . value)
                                in '(("JGT" . 1)("JEQ" . 2)("JGE" . 3)("JLT" . 4)
                                     ("JNE" . 5)("JLE" . 6)("JMP" . 7))
                                do (setf (gethash key hash) value))
                          hash) :accessor jumpcodes) ))

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

; To cut down on duplicate letting, a simple macro:
(defmacro forcmd (&body body)
  `(let ((cmd (current p)))
     ,@body))

(defmethod commandType ((p parse))
  (forcmd (cond ((stringstart cmd "@") 'A)
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
  (forcmd (cond ((stringstart cmd "@") (subseq cmd 1))
                ((stringstart cmd "(") (subseq cmd 1 (- (length cmd) 2)))
                (t (error "Can't get symbol from ~a" cmd)))))

; The parse & code methods
; Parse versions extract the string portion
; Code versions return binary data (as string)
(defmethod dest ((p parse))
  (forcmd (let ((eqpos (search "=" cmd)))
            (if eqpos
              (subseq cmd 0 eqpos)
              nil))))

(defmethod dest ((c code))
  (let ((cmd (d c)))
    (if cmd
      (num->bin (+ (if (search "M" cmd) 1 0)
                   (if (search "D" cmd) 2 0)
                   (if (search "A" cmd) 4 0))
                3)
      "000")))

(defmethod comp ((p parse))
  (forcmd
    ; C-command format is foo=bar;baz
    ; but only 'bar' is gauranteed
    (let ((eqpos (+ 1 (or (search "=" cmd) -1)))
          (semipos (or (search ";" cmd) (length cmd))))
      (subseq cmd eqpos semipos))))

(defmethod comp ((c code))
  (gethash (c c) *mnemonics*))

(defmethod jump ((p parse))
  (forcmd (let ((semipos (search ";" cmd)))
            (if semipos
              (subseq cmd (+ 1 semipos) (length cmd))
              nil))))

(defmethod jump ((c code))
  (let ((cmd (j c)))
    (if cmd
      (num->bin (gethash cmd (jumpcodes *parse*))
                3)
      "000")))

; Objects defined, do the thing
(defvar *parse* (make-instance 'parse :file (first *args*)))

; We need a mnemonic conversion table. Stick it in a hash
(defvar *mnemonics* (make-hash-table :test 'equal))

; To drastically reduce repetition, use a function to populate the hash
(defun putmnem (pairs)
  (loop for (key value) on pairs by #'cddr
        do (setf (gethash key *mnemonics*) value)))
(putmnem '("0"   "0101010" "1"   "0111111" "-1"  "0111010" "D"   "0001100"
           "A"   "0110000" "!D"  "0001101" "!A"  "0110001" "-D"  "0001111"
           "-A"  "0110011" "D+1" "0011111" "A+1" "0110111" "D-1" "0001110"
           "A-1" "0110010" "D+A" "0000010" "D-A" "0010011" "A-D" "0000111"
           "D&A" "0000000" "D|A" "0010101" "M"   "1110000" "!M"  "1110001"
           "-M"  "1110011" "M+1" "1110111" "M-1" "1110010" "D+M" "1000010"
           "D-M" "1010011" "M-D" "1000111" "D&M" "1000000" "D|M" "1010101"))

(defvar *command* nil)

(defmethod processCommand ((type (eql 'C)))
  (let ((code (make-instance 'code
                             :dest (dest *parse*)
                             :comp (comp *parse*)
                             :jump (jump *parse*))))
    (format t "111~a~a~a~%" (comp code) (dest code) (jump code))))

(defmethod processCommand (type)
  (format t "0~a~%" (num->bin (parse-integer (symbol *parse*)) 15)))

(loop for line = (current *parse*)
      while (hasMoreCommands *parse*)
      do (progn
           (processCommand (commandType *parse*))
           (advance *parse*)))
