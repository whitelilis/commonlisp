(in-package :wizard-util)


(defmacro easy-class (class-name super-classes slots)
  `(defclass ,class-name ,super-classes
     (list ,slots)))
