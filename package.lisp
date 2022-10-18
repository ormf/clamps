;;;; package.lisp

(defpackage #:cl-poolplayer
  (:shadowing-import-from #:cm :range :cd :pwd :drunk-traverse)
  (:use #:cl #:orm-utils #:cm #:of-incudine-dsps) ;;  #:incudine.scratch
  (:export
   #:*POOL-HASH*
   #:DTIME-DEV #:DTIME #:PRESET-PLAY #:EVENTPLAYER #:EVENTPLOTTER #:R-ELT
   #:NPRESET-PLAY
   #:FIG12-OUT #:LOAD-POOLPLAYER-SOUNDS
   #:COLLECT-POOL
   #:MAKE-SONG
   #:N-LIN #:N-EXP #:R-LIN #:R-EXP
   #:INIT-POOLPLAYER
   #:BUF-IDX
   #:PLAY-SONG
   #:STEREO-OUT
   #:SONG-PLAYFN
   #:SONG-DURFN
   #:SONG-NAME
   #:SONG-BEFOREFN
   #:SONG-AFTERFN
   #:DIGEST-POOLPLAYER-PRESET
   #:DISTRIBUTED-PLAY
   #:GET-DTIME-FN
   #:GET-DTIME-FN-NO-X
   #:VALUE-FN
   #:X
   #:ARGS
   #:P1 #:P2 #:P3 #:P4
   #:G1 #:G2 #:G3 #:G4
   #:CM-COLLECT
   ))
