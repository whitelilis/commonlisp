(in-package :wizard-util)

(defmacro with-log (stream &body forms)
  `(progn
     ,@(loop for f in forms
          collect `(format ,stream "~a : ~a -> ~a~%"  (now-ft) ',f ,f))))
