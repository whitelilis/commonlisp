;;;; wizard-util.asd

(asdf:defsystem #:wizard-util
  :serial t
  :depends-on (:cl-ppcre)

  :components ((:file "package")
               (:file "wizard-util")
               (:file "wizard-log")))
