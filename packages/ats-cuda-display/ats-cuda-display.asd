(asdf:defsystem #:ats-cuda-display
  :description "Browser display for ATS files."
  :author "Orm Finnendahl"
  :license  "Public Domain"
  :version "0.0.1"
  :serial t
  :depends-on (#:ats-cuda #:orm-utils #:clog-dsp-widgets #:svg-import-export)
  :components ((:file "package")
               (:file "ats-track-load-save")
               (:file "ats-svg-export")
               (:file "browser-gui")))
