(defvar *debug-output* t) 

(define-condition progn-debug-output (simple-condition) 
  ((form :initarg :form) 
   (multiple-value-list :initarg :multiple-value-list))) 

(defun progn-debugger (condition value) 
  (declare (ignorable value)) 
  (typecase condition 
    (progn-debug-output 
      (with-slots (form multiple-value-list) condition 
	(when *debug-output* 
	  (cond 
	    ((and (= (length multiple-value-list) 1) 
		  (eql form (first multiple-value-list))) 
	     (format *debug-output* "  ~S~%" form)) 
	    (t 
	      (format *debug-output* "  ~S -> ~{~S~^,~}~%" form multiple-value-list)))) 
	(signal condition))))) 

(defmacro progn-debug (&body body) 
  (let ((completedp (gensym "completed")) 
	(values (gensym "values"))) 
    `(let ((*debugger-hook* #'progn-debugger) 
	   (,completedp nil)) 
       (unwind-protect 
	 (progn 
	   (when *debug-output* 
	     (format *debug-output* "(PROGN-DEBUG~%")) 
	   (let ((,values 
		   (progn 
		     ,@(mapcar 
			 (lambda (form) 
			   `(handler-case 
			      (invoke-debugger 
				(make-condition 'progn-debug-output 
						:form ',form 
						:multiple-value-list 
						(multiple-value-list ,form))) 
			      (progn-debug-output (condition) 
						  (slot-value condition 'multiple-value-list)))) 
			 body)))) 
	     (setf ,completedp t) 
	     (apply #'values ,values))) 
	 (when *debug-output* 
	   (unless ,completedp 
	     (format *debug-output* "  #<Abnormal termination of PROGN-DEBUG>~%")) 
	   (format *debug-output* ")~%")))))) 


(defmacro debug-prt (tag &rest sexps)
  `(progn
     ,@(progn
	 (format t "~&debug-prt> start ~a" tag)
	 (mapcar (lambda (s)
		   (format t "~&debug-prt> eval: ~a=~a" s (eval s)))
		 sexps))
     (format t "~&debug-prt> end ~a" ,tag)))
