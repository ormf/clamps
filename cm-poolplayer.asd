;;;; cm-poolplayer.asd

(asdf:defsystem #:cm-poolplayer
  :description "common music bindings for cl-poolplayer"
  :author "Orm Finnendahl <orm.finnendahl@selma-hfmdk-frankfurt.de"
  :license  "GPL 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-poolplayer #:cm-svg #:cm-utils)
  :components ((:file "cm-poolplayer")))
