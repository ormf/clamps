;;;; package.lisp

(defpackage #:cl-midictl
  (:use #:cl #:incudine)
  (:export #:START-MIDI-RECEIVE
           #:STOP-MIDI-RECEIVE
           #:ENSURE-CONTROLLER
           #:ADD-MIDI-CONTROLLER
           #:REMOVE-MIDI-CONTROLLER
           #:MIDI-CONTROLLER
           #:FIND-CONTROLLER
           #:NANOCTL
           #:GUI
           #:*MIDI-DEBUG*))
