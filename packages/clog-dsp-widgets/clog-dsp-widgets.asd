;;;; clog-dsp-widgets.asd
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:clog-dsp-widgets
  :description "Describe clog-dsp-widgets here"
  :depends-on (:yason :clog :cl-refs :incudine)
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "clog-redefs")
               (:file "clog-dsp-widgets")))
