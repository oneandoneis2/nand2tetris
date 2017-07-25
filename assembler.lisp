#!/usr/bin/clisp

(defvar *source* ())

(with-open-file (src (first *args*))
  (setf *source*
        (loop for line = (read-line src nil)
              while line
              collect line)))

(princ *source*)
