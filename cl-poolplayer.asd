;;;; cl-poolplayer.asd

(asdf:defsystem #:cl-poolplayer
  :description "Poolplayer derived from big orchestra project."
  :author "Orm Finnendahl"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:incudine-bufs #:of-incudine-dsps #:cl-sfz #:orm-utils #:cm-utils)
  :components ((:file "package")
               (:file "globals")
               (:file "classes")
               (:file "envelope")
               (:file "output")
               (:file "utils")
               (:file "presets")
               (:file "load-sounds")
               (:file "network")
               (:file "cl-poolplayer")
;;;               (:file "sequencer")
;;;               (:file "midictl")
;;;               (:file "cm-poolplayer")
;;;               (:file "cl-poolplayer-default-presets")
               (:file "init")
               )) 
