;;;; package.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(defpackage #:clog-midi-controller
  (:use #:cl #:ou #:cl-refs #:cl-midictl #:clog #:clog-dsp-widgets #:incudine)
  (:shadowing-import-from #:incudine
                          #:group)
  (:shadowing-import-from #:alexandria
                          #:rotate)
  (:export #:NANOKTL2-GUI #:FADERFOX-GUI #:NANOKTL2-PRESET-GUI
           #:MIDICONTROLLER
           #:GUI-PARENT #:GUI-CONTAINER #:GUI-FADER #:GUI-S-BUTTONS #:GUI-M-BUTTONS #:GUI-R-BUTTONS
           #:GUI-CTL-PANEL #:CTL-PANEL-VIS
           #:GUI-TRACK-LEFT #:GUI-TRACK-RIGHT #:GUI-CYCLE #:GUI-SET-MARKER #:GUI-MARKER-LEFT #:GUI-MARKER-RIGHT
           #:GUI-REWIND #:GUI-FFWD #:GUI-STOP #:GUI-PLAY #:GUI-REC))
