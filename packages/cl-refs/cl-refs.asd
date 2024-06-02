;;;; cl-refs.asd
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:cl-refs
  :description "Describe cl-refs here"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :depends-on (#:atomics)
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-refs")))
