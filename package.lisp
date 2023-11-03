;;;; package.lisp

(defpackage #:cl-midictl
  (:use #:cl #:cellctl #:incudine)
  (:export #:START-MIDI-RECEIVE
           #:STOP-MIDI-RECEIVE
           #:ENSURE-CONTROLLER
           #:ADD-MIDI-CONTROLLER
           #:REMOVE-MIDI-CONTROLLER
           #:REMOVE-ALL-MIDI-CONTROLLERS
           #:MIDI-CONTROLLER
           #:FIND-CONTROLLER
           #:NANOCTL-MIDI
           #:GUI
           #:CHAN
           #:*MIDI-DEBUG*
           #:*MIDI-IN1*
           #:*MIDI-OUT1*
           #:CC-MAP #:ID #:CHAN #:MIDI-INPUT #:MIDI-OUTPUT #:LAST-NOTE-ON #:CC-STATE #:CC-FNS #:NOTE-FNS

           ))
