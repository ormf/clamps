;;;; package.lisp

(defpackage #:of-incudine-dsps
  (:use #:cl #:ou #:incudine :incudine.vug :incudine.util :incudine.analysis)
  (:nicknames #:oid)
  (:shadowing-import-from #:incudine #:GROUP)
  (:shadow #:clip)
  (:export #:BUFFER-RECORD
           #:ENVELOPE*
           #:LINE*
           #:PHASOR*
           #:PHASOR-LOOP*
           #:BUFFER-PLAY*
           #:BUFFER-LOOP-PLAY*
           #:BUFFER-STRETCH-PLAY*
           #:PLAY-BUFFER-STRETCH*
           #:PLAY-BUFFER-LOOP*
           #:PLAY-BUFFER-STRETCH
           #:PLAY-BUFFER-STRETCH-OUT
           #:PLAY-BUFFER-STRETCH-ENV-OUT
           #:PLAY-BUFFER-STRETCH-ENV-PAN-OUT
           #:PLAY-BUFFER-STRETCH-ENV-PAN-OUT*
           #:PLAY-BUFFER*
           #:*HANNING1024*
           #:*SINE1024*
           #:MAKE-OASR
           #:*ENV1*
           #:COUNTER
           #:OSC~
           #:LSAMPLE
           #:MAKE-LSAMPLE
           #:CREATE-LSAMPLE
           #:LSAMPLE-NAME
           #:LSAMPLE-BUFFER
           #:LSAMPLE-ONESHOT
           #:LSAMPLE-KEYNUM
           #:LSAMPLE-LOOPSTART
           #:LSAMPLE-AMP
           #:LSAMPLE-LOOPEND
           #:LSAMPLE-PATHNAME
           #:LSAMPLE-DUR
           #:PLAY-LSAMPLE
           #:PLAY-SAMPLE
           #:RESTORE-ENVS
           #:KEYNUM->HZ
           #:*KEYNUM-OFFSET*
           #:METERS
           #:INPUT-BUS
           #:BUS-VALUE
;;;           #:NODE-FREE-UNPROTECTED
           #:CLEAR-BUSES
           #:CP-INPUT-BUSES
           #:CP-OUTPUT-BUSES
           #:CLEAR-BUSES
           #:MIX-BUS-TO-OUT
           #:BUS-TO-OUT
           ))
