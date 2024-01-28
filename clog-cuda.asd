;;;; clog-cuda.asd
;;;
;;; extension to clog-dsp-widgets for incudine
;;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:clog-cuda
  :description "clog widgets for use with incudine"
  :depends-on (:yason :clog :of-incudine-dsps :clog-dsp-widgets :cl-refs)
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :components (;;; (:file "package")
               (:file "clog-cuda")))
