;;;; package.lisp

(defpackage #:cl-poolplayer
  (:use #:cl #:orm-utils #:cm #:incudine #:of-incudine-dsps) ;;  #:incudine.scratch
  (:shadowing-import-from #:cm :range :cd :pwd :drunk-traverse :quantize
                          #:at #:rescale-envelope #:tuning #:play
                          #:now #:stop #:*tempo*
                          )
  (:shadowing-import-from #:orm-utils #:group)
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
   #:*CIRCLE-CW*
   #:*OUTSEQ8*
   #:*OUTSEQ9*
   #:*OUTSEQ13*
   #:EVENTPLAYER
   #:EVENTPLOTTER
   #:*POOLPLAYER-PRESETS-FILE*
   #:SHOW-POOLPLAYER-PRESET
   #:LOAD-POOLPLAYER-PRESETS
   #:SAVE-POOLPLAYER-PRESETS))
