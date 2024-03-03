;;;; cm-all.asd

(if (find-package :slynk) (pushnew :slynk *features*))
(if (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:cm-all
  :description "cm with all additions loaded"
  :author "Orm Finnendahl <ormfinnendahl@selma.hfmdk-frankfurt.de"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (
               #:incudine
               #:of-incudine-dsps ;;; #:sol
               #:incudine-bufs
               #:orm-utils
               #:cl-plot
               #:incudine-plot
               #:cl-ppcre
               #:cl-refs
               #:cl-sfz
               #:cm
               #:cm-sfz
               #:cm-poolevt
               #:cm-fomus
               #:cm-svg
               #:cm-svg.rts
               #:cm-incudine
               #:cm-utils
               #:clog-dsp-widgets
               #:ats-cuda
               #:cl-midictl
               #:clog-midi-controller
               )
  :components ((:module "src"
                :serial t
                :components
                (
                 (:file "package")
                 (:file "cm-all")
                 
                 ))))
