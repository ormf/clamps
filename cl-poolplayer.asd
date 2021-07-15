;;;; cl-poolplayer.asd

(asdf:defsystem #:cl-poolplayer
  :description "Poolplayer derived from big orchestra project."
  :author "Orm Finnendahl"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cm-utils :of-incudine-dsps :orm-utils)
  :components ((:file "package")
               (:file "globals")
               (:file "classes")
               (:file "envelope")
               (:file "utils")
               (:file "output")
               (:file "presets")
               (:file "load-sounds")
               (:file "network")
               (:file "cl-poolplayer")
               (:file "sequencer")
               (:file "midictl")
               (:file "cm-poolplayer")
               (:file "init")
               )) 
