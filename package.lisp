;;;; package.lisp

(defpackage #:cl-midictl
  (:use #:cl #:ou #:cellctl #:incudine)
  (:shadowing-import-from #:incudine
                          #:group)
  (:export #:START-MIDI-RECEIVE
           #:STOP-MIDI-RECEIVE
           #:ENSURE-CONTROLLER
           #:ADD-MIDI-CONTROLLER
           #:REMOVE-MIDI-CONTROLLER
           #:REMOVE-ALL-MIDI-CONTROLLERS
           #:MIDI-CONTROLLER
           #:FIND-CONTROLLER
           #:NANOKTL2-MIDI
           #:GUI
           #:ECHO
           #:CHAN
           #:*MIDI-DEBUG*
           #:*MIDI-IN1*
           #:*MIDI-OUT1*
           #:*MIDI-CC-STATE*
           #:*OSCIN*
           #:START-OSC-MIDI-RECEIVE
           #:STOP-OSC-MIDI-RECEIVE
           #:OSC-MIDI-WRITE-SHORT
           #:CC-MAP #:ID #:CHAN #:MIDI-INPUT #:MIDI-OUTPUT #:LAST-NOTE-ON #:CC-STATE #:CC-FNS #:NOTE-STATE #:NOTE-FNS
           #:TOGGLE-SLOT
           #:NK2-FADERS #:NK2-FADER-UPDATE-FNS #:S-BUTTONS #:M-BUTTONS #:R-BUTTONS #:TRACK-LEFT #:TRACK-RIGHT
           #:CYCLE #:SET-MARKER #:MARKER-LEFT #:MARKER-RIGHT #:TR-REWIND #:TR-FFWD #:TR-STOP #:TR-PLAY #:TR-REC #:CC-NUMS

           #:FADERFOX-MIDI
           #:FF-FADERS
           #:FF-BUTTONS
           #:UPDATE-STATE))
