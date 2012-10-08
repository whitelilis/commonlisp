(defclass sim-queue ()
  ((messages :accessor messages :initarg :messages :initform nil)
   (last-cons :accessor last-cons :initarg :last-cons :initform nil
              :documentation "Cached end of the list")
   (len :accessor len :initarg :len :initform 0
        :documentation "Cached message queue length. Modified by enqueue and dequeue")
   (lock :initform (make-lock) :accessor lock
         :documentation "Lock for this message queue")
   (max-len :accessor max-len :initarg :max-len :initform nil
            :documentation "If present, queue maintains at most this many elements")))

(defun make-queue (&optional max-len)
  (make-instance 'sim-queue :max-len max-len))

(defmethod full-p ((queue sim-queue))
  (with-slots (len max-len lock) queue
    (with-lock-grabbed (lock)
      (and max-len (>= len max-len)))))

(defmethod empty-p ((queue sim-queue))
  (with-slots (len lock) queue
    (with-lock-grabbed (lock)
      (= (len queue) 0))))

(defmethod enqueue (object (queue sim-queue))
  "Adds an element to the back of the given queue in a thread-safe way. If full, dequeue"
  (with-slots (lock messages max-len len last-cons) queue
    (with-lock-grabbed (lock)
      (let ((o (list object)))
        (cond ((empty-p queue)
               (setf messages o
                     last-cons messages
                     len 1))
              ((full-p queue)
               (pop messages)
               (setf (cdr last-cons) o
                     last-cons o))
              (t (setf (cdr last-cons) o
                       last-cons o)
                 (incf len)))))
    (values messages len)))

(defmethod dequeue ((queue sim-queue))
  "Pops a message from the given queue in a thread-safe way."
  (with-slots (messages lock len) queue
    (with-lock-grabbed (lock)
      (if (= len 0)
          (values nil nil)
          (progn
            (decf len)
            (values (pop messages) len))))))

