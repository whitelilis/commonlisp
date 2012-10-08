(defpackage :wizard-thread-pool
  (:use :cl :cl-user :sb-thread))

(require :log4cl)

(log:config :sane :daily "/home/wizard/mmm.log")

(in-package :wizard-thread-pool)


(defclass sim-queue ()
  ((messages :accessor messages :initarg :messages :initform nil)
   (last-cons :accessor last-cons :initarg :last-cons :initform nil
              :documentation "Cached end of the list")
   (len :accessor len :initarg :len :initform 0
        :documentation "Cached message queue length. Modified by enqueue and dequeue")
   (lock :initform (make-mutex) :accessor lock
         :documentation "Lock for this message queue")
   (cond-var :accessor cond-var :initarg :cond-var :initform (make-waitqueue))
   (max-len :accessor max-len :initarg :max-len :initform nil
            :documentation "If present, queue maintains at most this many elements")))

(defun make-sim-queue (&optional max-len)
  (make-instance 'sim-queue :max-len max-len))

(defmethod full-p ((queue sim-queue))
  (with-slots (len max-len lock) queue
    (with-recursive-lock (lock)
      (and max-len (>= len max-len)))))

(defmethod empty-p ((queue sim-queue))
  (with-slots (len lock) queue
    (with-recursive-lock (lock)
      (= (len queue) 0))))

(defmethod enqueue (object (queue sim-queue))
  "Adds an element to the back of the given queue in a thread-safe way. If full, dequeue"
  (with-slots (lock messages max-len len last-cons) queue
    (with-recursive-lock (lock)
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
    (with-recursive-lock (lock)
      (if (= len 0)
          (values nil nil)
          (progn
            (decf len)
            (values (pop messages) len))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct wizard-thread-pool
  (task-queue (make-sim-queue))
  running-list)


(defun loop-run-thread (task-queue name)
  (with-slots (lock cond-var len) task-queue
    (loop
       (grab-mutex lock)
       (loop until (> len 0)
          do (condition-wait cond-var lock))
       (let ((task (dequeue task-queue)))
         (release-mutex lock)
         (when task
           (log:info "~a start" name)
           (apply (car task) (cdr task))
           (log:info "~a finished" name))))))


(defun make-thread-pool (&optional (pool-size 10))
  (let ((ret (make-wizard-thread-pool)))
    (dotimes (i pool-size)
      (push (make-thread #'loop-run-thread :arguments (list (wizard-thread-pool-task-queue ret) (format nil "thread-~a" i))) (wizard-thread-pool-running-list ret)))
    ret))


(defun new-task (wizard-thread-pool appliable-task)
  (with-slots (lock cond-var) (wizard-thread-pool-task-queue wizard-thread-pool)
    (with-recursive-lock (lock)
      (enqueue appliable-task (wizard-thread-pool-task-queue wizard-thread-pool))
      (condition-broadcast cond-var))))



(defun thread-test (sleep-second)
  (log:info "start sleep ~a second" sleep-second)
  (sleep sleep-second)
  (log:info "finish sleep ~a second" sleep-second))

(defun destory-pool (thread-pool)
  (dolist (thread (wizard-thread-pool-running-list thread-pool))
    (terminate-thread thread)))


(defparameter *pool-test* (make-thread-pool 20))


(dotimes (i 70)
  (new-task *pool-test* (list #'thread-test (random 70))))
