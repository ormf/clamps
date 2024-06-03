;;; clamps.asd

(asdf:defsystem #:clamps
  :description "Common Lisp Aided Music Production System"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:cl-refs
               #:orm-utils
               #:incudine
               #:cm
               #:fomus
               #:ats-cuda)
  :components ((:file "load-packages")
               (:file "clamps")))
