;;;; cl-midictl.asd

(asdf:defsystem #:clamps-sensors
  :description "Abstraction layer handling motion tracking sensors
in clamps."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-refs #:orm-utils #:clog-dsp-widgets #:incudine)
  :components ((:file "package")
               (:file "sensors")))
