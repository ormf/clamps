;;;; package.lisp

(defpackage #:cl-midictl
  (:use #:cl #:ou #:cl-refs #:incudine)
  (:shadow #:id)
  (:shadowing-import-from #:cm
                          #:chan
                          #:*midi-in1*
                          #:*midi-out1*)
  (:shadowing-import-from #:incudine
                          #:group)
  (:export
   #:CLEANUP
   #:*MIDI-IN1*
   #:*MIDI-OUT1*
   #:START-MIDI-RECEIVE
   #:STOP-MIDI-RECEIVE
   #:ADD-MIDI-CONTROLLER
   #:LIST-MIDI-CONTROLLERS
   #:REMOVE-MIDI-CONTROLLER
   #:REMOVE-ALL-MIDI-CONTROLLERS
   #:MIDI-CONTROLLER
   #:CCIN
   #:MCTL-ID
   #:FIND-CONTROLLER
   #:CLOSE-MIDI-PORT
   #:FIND-MIDI-PORT
   #:LIST-MIDI-PORTS
   #:OPEN-MIDI-PORT
   #:MIDI-PORT
   #:MIDI-PORT-ID
   #:MIDI-PORT-INPUT
   #:MIDI-PORT-OUTPUT
   #:MIDI-PORT-CC-STATE
   #:MIDI-PORT-NOTE-STATE
   #:MIDI-PORT-PITCH-BEND-STATE
   #:MIDI-PORT-AFTER-TOUCH-STATE
   #:MIDI-PORT-CC-FNS
   #:MIDI-PORT-NOTE-FNS
   #:MIDI-PORT-PITCH-BEND-FNS
   #:MIDI-PORT-AFTER-TOUCH-FNS
   #:NANOKTL2-MIDI
   #:NANOKTL2-PRESET-MIDI
   #:*NANOKTL2-PRESETS-FILE*
   #:SAVE-PRESETS
   #:LOAD-PRESETS
   #:SET-PLAYER-BUTTONS
   #:INIT-NK2
   #:GUI
   #:ECHO
   #:CHAN
   #:*MIDI-DEBUG*
   #:*DEFAULT-MIDI-CHANNEL*
   #:*DEFAULT-MIDI-PORT*
   #:*MIDI-CC-STATE*
   #:*MIDI-CC-FNS*
   #:*MIDI-NOTE-STATE*
   #:*MIDI-NOTE-FNS*
   #:ADD-MIDI-CC-FN
   #:REMOVE-MIDI-CC-FNS
   #:REMOVE-ALL-CHANNEL-MIDI-CC-FNS
   #:REMOVE-ALL-MIDI-CC-FNS
   #:SHOW-MIDI-CC-FNS
   #:*OSCIN*
   #:START-OSC-MIDI-RECEIVE
   #:STOP-OSC-MIDI-RECEIVE
   #:OSC-MIDI-WRITE-SHORT
   #:CC-MAP #:ID #:CHAN #:MIDI-INPUT #:MIDI-OUTPUT #:LAST-NOTE-ON #:CC-STATE #:CC-FNS #:NOTE-STATE #:NOTE-FNS
   #:TOGGLE-SLOT
   #:HANDLE-MIDI-IN
   #:NK2-FADERS #:NK2-LAST-CC
   #:NK2-FADER-MODES
   #:NK2-FADER-UPDATE-FNS #:S-BUTTONS #:M-BUTTONS #:R-BUTTONS #:TRACK-LEFT #:TRACK-RIGHT
   #:BANK-BUTTONS
   #:CYCLE #:SET-MARKER #:MARKER-LEFT #:MARKER-RIGHT #:TR-REWIND #:TR-FFWD #:TR-STOP #:TR-PLAY #:TR-REC #:CC-NUMS
   #:GUI #:GUI-UPDATE-OFF #:WITH-GUI-UPDATE-OFF
   #:FADERFOX-MIDI
   #:CURR-PLAYER
   #:FADERFOX-MIDI-F.ORM
   #:UPDATE-HW-STATE
   #:UPDATE-ALL-CONTROLLERS
   #:UPDATE-PRESET-BUTTONS
   #:HANDLE-PLAYER-SWITCH
   #:GET-REF
   #:START-MIDI-ENGINE
   #:PRESETS
   #:UNWATCH
   #:BUCHLA-SCALE
   #:MIDI->INC
   #:SHOW-MIDI-CC-FNS
   #:SHOW-MIDI-CC-FNS
   #:TOGGLE-REF-WATCH
   #:MAKE-LED-PULSAR
   #:COLOR->MIDI-RGB
   #:BYTE->MIDI
   #:NORMALIZED->BENDVALUE
   #:NANOKTL2-MIDI
   #:NK2-FADERS
   #:NK2-FADER-TEST-SYNC-FNS
   #:NK2-FADER-MODES
   #:NK2-FADER-LAST-CC
   #:HIDE-FADER
   #:S-BUTTONS
   #:M-BUTTONS
   #:TRACK-LEFT
   #:TRACK-RIGHT
   #:NK-CYCLE
   #:SET-MARKER
   #:MARKER-LEFT
   #:MARKER-RIGHT
   #:TR-REWIND
   #:TR-FFWD
   #:TR-STOP
   #:TR-PLAY
   #:TR-REC
   #:CC-NUMS
   ))
