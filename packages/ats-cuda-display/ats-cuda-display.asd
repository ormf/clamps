(asdf:defsystem #:ats-cuda-display
  :description "ATS file parser for Common Lisp
  and port of Juan Pampin's ats lisp code, incudine version."
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ats-cuda)
  :components ((:file "browser-gui")))
