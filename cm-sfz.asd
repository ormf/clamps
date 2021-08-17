;;;; cm-sfz.asd

(asdf:defsystem #:cm-sfz
  :description "common music 2.0 bindings for cl-szz"
  :author "Orm Finnendahl <orm.finnendahl@selma-hfmdk-frankfurt.de"
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-sfz #:cm-svg #:cm-utils)
  :components ((:file "cm-sfz")))
