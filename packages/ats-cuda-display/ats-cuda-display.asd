(asdf:defsystem #:ats-cuda-display
  :description "Browser display for ATS files."
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ats-cuda)
  :components ((:file "package")
               (:file "browser-gui")))
