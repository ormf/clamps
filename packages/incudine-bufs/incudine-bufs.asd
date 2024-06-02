;;;; incudine-bufs.asd

(asdf:defsystem #:incudine-bufs
  :description "Describe incudine-bufs here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine #:cl-ppcre)
  :components ((:file "package")
               (:file "incudine-bufs")))
