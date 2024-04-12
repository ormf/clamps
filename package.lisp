;;;; package.lisp

(defpackage #:cl-poolplayer
  (:use #:cl #:orm-utils #:incudine #:of-incudine-dsps #:incudine-bufs) ;;  #:incudine.scratch
  (:shadowing-import-from #:cm :song #:new :range :cd :pwd
;;;                                                  :drunk-traverse
   :quantize
                          #:at #:rescale-envelope #:tuning #:play
                          #:now #:stop #:*tempo*
                          #:next #:cycle #:interp
                          #:weighting
                          )
  (:shadowing-import-from #:orm-utils #:group)
  (:export
   #:*POOL-HASH*
   #:DTIME-DEV #:DTIME #:PRESET-PLAY #:EVENTPLAYER #:EVENTPLOTTER #:R-ELT
   #:NPRESET-PLAY
   #:FIG12-OUT #:LOAD-POOLPLAYER-SOUNDS
   #:COLLECT-POOL
   #:MAKE-P-SONG
   #:N-LIN #:N-EXP #:R-LIN #:R-EXP
   #:INIT-POOLPLAYER
   #:BUF-IDX
   #:PLAY-SONG
   #:STEREO-OUT
   #:PERFORM
   #:P-SONG-PLAYFN
   #:P-SONG-DURFN
   #:P-SONG-NAME
   #:P-SONG-BEFOREFN
   #:P-SONG-AFTERFN
   #:DIGEST-POOLPLAYER-PRESET
   #:FN-DIGEST-POOLPLAYER-PRESET
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
   #:PREVIOUS-POOLPLAYER-PRESET
   #:NEXT-POOLPLAYER-PRESET
   #:SHOW-POOLPLAYER-PRESET
   #:LOAD-POOLPLAYER-PRESETS
   #:SAVE-POOLPLAYER-PRESETS
   #:SERIALIZE-SCORE
   #:SET-BASEDIR
   #:*POOLPLAYER-EVENTS*
   ))
