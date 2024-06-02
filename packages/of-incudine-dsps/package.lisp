;;;; package.lisp

(defpackage #:of-incudine-dsps
  (:use #:cl #:ou #:incudine :incudine.vug :incudine.util :incudine.analysis)
  (:nicknames #:oid)
  (:shadowing-import-from #:incudine #:GROUP)
  (:shadow #:clip)
  (:export #:BUFFER-STRETCH-PLAY
           #:PLAY-BUFFER-STRETCH*
           #:PLAY-BUFFER-STRETCH
           #:PLAY-BUFFER-STRETCH-OUT
           #:PLAY-BUFFER-STRETCH-ENV-OUT
           #:PLAY-BUFFER-STRETCH-ENV-PAN-OUT
           #:PLAY-BUFFER-STRETCH-ENV-PAN-OUT*
           #:PLAY-BUFFER*
           #:*HANNING1024*
           #:MAKE-OASR
           #:*ENV1*
           #:MAKE-LSAMPLE
           #:LSAMPLE
           #:PLAY-LSAMPLE*
           #:PLAY-SAMPLE*
           #:ENVELOPE*
           #:PHASOR*
           #:LINE*
           #:PHASOR-LOOP*
           #:BUFFER-LOOP-PLAY*
           #:LSAMPLE-FILENAME
           #:LSAMPLE-BUFFER
           #:LSAMPLE-PLAY-FN
           #:LSAMPLE-KEYNUM
           #:LSAMPLE-LOOPSTART
           #:LSAMPLE-AMP
           #:LSAMPLE-LOOPEND
           #:PLAY-LSAMPLE
           #:PLAY-SAMPLE
           #:RESTORE-ENVS
           ;; #:METERS
           ;; #:INPUT-BUS
           ;; #:NODE-FREE-UNPROTECTED
           ;; #:CLEAR-BUSES
           ;; #:CP-INPUT-BUSES
           ;; #:CP-OUTPUT-BUSES
           ;; #:CLEAR-BUSES
           ;; #:MIX-BUS-TO-OUT
           ;; #:BUS-TO-OUT
           ))
