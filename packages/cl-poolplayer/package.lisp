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
  (:shadowing-import-from #:orm-utils #:group #:clip)
  (:shadowing-import-from #:cl-midictl #:ccin)
  (:export
   #:*POOL-HASH*
   #:DTIME-DEV #:DTIME #:PRESET-PLAY #:EVENTPLAYER #:EVENTPLOTTER #:R-ELT
   #:NPRESET-PLAY
   #:FIG12-OUT #:LOAD-POOLPLAYER-SOUNDS
   #:COLLECT-POOL
   #:MAKE-P-SONG
;;;   #:N-LIN #:N-EXP #:R-LIN #:R-EXP
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
   #:DIGEST-FORM-TO-PRESET
   #:DIGEST-INTERP-FORM
   #:EDIT-PRESET-IN-EMACS
   #:GET-INTERP-VALS
   #:IP-IDX #:IP-NUM
   #:FN-DIGEST-POOLPLAYER-PRESET
   #:DISTRIBUTED-PLAY
   #:GET-DTIME-FN
   #:GET-PARAM-FNS
   #:PARAMS-FN
   #:DTIME-FN
   #:PRESET-FORM
   #:*DEFAULT-PARAM-FNS*
   #:*PARAM-LOOKUP*
   #:GET-DTIME-FN-NO-X
   #:VALUE-FN
   #:X
   #:IDX
   #:ARGS
   #:P1 #:P2 #:P3 #:P4
   #:G1 #:G2 #:G3 #:G4
   #:CM-COLLECT
   #:*CIRCLE-CW*
   #:*OUTSEQ8*
   #:*OUTSEQ9*
   #:*OUTSEQ13*
   #:MAKE-EVENTPLAYER
   #:*POOLPLAYER-PRESETS-FILE*
   #:*POOLPLAYER-PRESETS*
   #:*CURR-POOLPLAYER-PRESET-NO*
   #:DEFINE-POOLPLAYER-ELISP-CODE
   #:PREVIOUS-POOLPLAYER-PRESET
   #:NEXT-POOLPLAYER-PRESET
   #:SHOW-POOLPLAYER-PRESET
   #:LOAD-POOLPLAYER-PRESETS
   #:SAVE-POOLPLAYER-PRESETS
   #:SET-POOLPLAYER-PRESET-FORM
   #:SERIALIZE-SCORE
   #:INIT-POOLPLAYER
   #:*POOLPLAYER-EVENTS*
   #:*POOLPLAYER-RECORDING-P*
   #:EXPAND-ARG-FORMS
   #:NORMALIZE-X
   #:PLAYING #:PRESET-NO #:ID #:START #:END #:DUR
   #:INIT-POOLPLAYER-ELISP-CODE))
