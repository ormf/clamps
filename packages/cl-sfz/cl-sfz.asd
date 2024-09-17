;;;; cl-sfz.asd

(asdf:defsystem #:cl-sfz
  :description "simple player for sfz soundfonts"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gnu public license 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine #:incudine-bufs #:of-incudine-dsps #:cl-ppcre)
  :components ((:file "package")
               (:file "sfz-lsample")
               (:file "cl-sfz")
               (:file "make-sfz")))
