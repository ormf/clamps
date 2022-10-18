;;;; cm-all.asd

(asdf:defsystem #:cm-all
  :description "cm with all additions loaded"
  :author "Orm Finnendahl <ormfinnendahl@selmahfmdk-frankfurt.de"
  :license  "lgpl"
  :version "0.0.1"
  :serial t
  :depends-on (#:cm-utils #:cl-ppcre #:of-incudine-dsps ;;; #:sol
                          #:cl-sfz #:cm-sfz #:cm-poolplayer)
  :components ((:file "package")
               (:file "cm-all")))
