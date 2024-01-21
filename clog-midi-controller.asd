;;;; clog-midi-controller.asd
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(asdf:defsystem #:clog-midi-controller
  :description "Gui code for Midi Controllers using CLOG."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:clog-widgets #:cl-midictl)
  :components ((:file "package")
               (:file "clog-midi-controller")
               (:file "nanoktl2-preset-gui")
;;;               (:file "faderfox-gui")
               ))
