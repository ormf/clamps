;;;; clog-widgets.asd
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:clog-widgets
  :description "Describe clog-widgets here"
  :depends-on (:yason :clog :cl-refs)
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "clog-widgets")))
