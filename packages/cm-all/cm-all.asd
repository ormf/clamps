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
               #:cl-refs
               #:incudine
               #:of-incudine-dsps
               #:incudine-bufs
               #:orm-utils
               #:cm-svg
               #:cl-plot
               #:incudine-plot
               #:cl-ppcre
               #:cl-sfz
               #:cm
               #:cm-sfz
               #:cm-poolevt
               #:cm-fomus
               #:cm-poolevt
               #:cm-poolplayer
               #:cm-incudine
               #:cm-utils
               #:clog-dsp-widgets
               #:ats-cuda
               #:clog-cuda
               #:cl-midictl
               #:clog-midi-controller
               )
  :components ((:module "src"
                :serial t
                :components
                (
                 (:file "package")
;;;                 (:file "cm-all")
                 
                 ))))
