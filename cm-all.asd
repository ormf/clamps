;;;; cm-all.asd

(if (find-package :slynk) (pushnew :slynk *features*))
(if (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:cm-all
  :description "cm with all additions loaded"
  :author "Orm Finnendahl <ormfinnendahl@selmahfmdk-frankfurt.de"
  :license  "lgpl"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:of-incudine-dsps ;;; #:sol
                          #:cl-refs
                          #:incudine-bufs
                          #:cl-sfz
                          #:cm-sfz
                          #:cm-poolplayer
                          #:cm-utils)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
;;;                 (:file "cm-all")
                 ))))
