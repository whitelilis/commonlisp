;;;; wizard-util.lisp

(in-package #:wizard-util)

;;; "wizard-util" goes here. Hacks and glory await!

(defun make-keyword (name)
  (if (keywordp name)
      name
      (values (intern (string-upcase name) "KEYWORD"))))

(defun now-ft ()
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format nil "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour min sec)))

(defun utime-ft(utime)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (or utime 0))
    (format nil "~a-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour min sec)))

(defun dis-percent (absolute-values)
  (let ((sum (reduce  #'+ (mapcar (lambda (x)
                                    (cdr x)) absolute-values))))
    (mapcar (lambda (x)
              (list (car x) (cdr x) (/ (cdr x) (* 0.01 sum)))) absolute-values)))


(defun circular-list (&rest elements)
  "Making circular list."
  (let ((backbone (copy-list elements)))
    (nconc backbone backbone)))

(defun package-relative-path (package name)
  (asdf:system-relative-pathname (make-keyword package) name))

(defun list2pec (lst)
           (let ((s (apply #'+ lst)))
             (loop for i in lst
                collect (/ i (* s 100.0)))))

(defmacro with-splited-line (filename local-let begin mid end &optional &key (seperator "\\s+"))
  `(let (,@local-let)
     (with-open-file (stream ,filename)
       ,@begin
       (loop for line = (read-line stream nil 'eof)
          until (eq line 'eof)
          do (let ((wizard-splited-line (cl-ppcre:split ,seperator line)))
               ,@mid
               ))
       ,@end)))

(defmacro with-follower (filename local-let begin mid)
  "Follow a file by name, still running. Designed for log parsing."
  `(let (,@local-let)
     (with-open-stream (stream (external-process-output-stream (run-program "/bin/bash" (list "-c" (format nil "~a ~a" "tail -F" ,filename)) :output :stream :wait nil)))
       ,@begin
       (loop for line = (read-line stream nil 'eof)
          until (eq line 'eof)
          do (progn
               ,@mid)))))

(defun single-class-item-transfer (item)
  (let ((name (if (consp item) (car item) item))
        (default-value (if (consp item) (cadr item) nil)))
    (list name :accessor name :initarg (intern (string-upcase (string name)) "KEYWORD") :initform default-value)))


(defmacro sim-class (classname super-classes slot-list)
  `(defclass ,classname ,super-classes
     ,(mapcar #'single-class-item-transfer slot-list)))
