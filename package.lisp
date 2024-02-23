;;;; package.lisp

(defpackage #:of-incudine-dsps
  (:use #:cl #:ou #:incudine :incudine.vug :incudine.util :incudine.analysis)
  (:nicknames #:oid)
  (:shadowing-import-from #:incudine #:GROUP)
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
           #:LSAMPLE-FILENAME
           #:LSAMPLE-BUFFER
           #:LSAMPLE-PLAY-FN
           #:LSAMPLE-KEYNUM
           #:LSAMPLE-LOOPSTART
           #:LSAMPLE-AMP
           #:LSAMPLE-LOOPEND
           #:PLAY-LSAMPLE
           #:PLAY-SAMPLE))

