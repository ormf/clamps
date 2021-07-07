;;;; cl-sfz.asd

(asdf:defsystem #:cl-sfz
  :description "Describe cl-sfz here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gnu public license 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-sfz")))
