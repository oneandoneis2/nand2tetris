#!/usr/bin/clisp

(defvar *source* ())

(with-open-file (src (first *args*))
  (loop for line = (read-line src nil)
        while line
        do (setf *source* (cons line *source*))))

(nreverse *source*)
(princ *source*)
