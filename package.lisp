;;; 
;;; package.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(defpackage :clamps
  (:export #:reset-logger-stream #:idump #:clamps #:clamps-restart-gui #:clamps-gui-root
           #:svg-gui-path #:set-tempo #:start-doc-acceptor #:clamps-start
           #:gui #:meters)
  (:shadowing-import-from #:incudine
                          #:rescale-envelope #:init #:stop
                          #:group #:*tempo* #:control-value
                          #:timestamp #:responder
;;; play-sample
                          )

  (:shadowing-import-from #:of-incudine-dsps #:clip)
  (:shadowing-import-from #:cl-poolplayer #:args)

  (:use #:cl #:cl-user #:cm #:of-incudine-dsps #:ou)
  (:shadow #:mtof #:ftom
           #:buffer #:buffer-frames
           #:buffer-sample-rate
           #:node #:bus
           #:lsample #:envelope
           #:lsample-keynum #:lsample-play-fn #:lsample-amp #:lsample-buffer
           #:lsample-buffer #:remove-all-responders #:recv-stop
           #:without-interrupts
           #:play)

  (:shadowing-import-from #:cl-midictl
                          #:*midi-in1* #:*midi-out1*
                          #:chan #:id)

  (:shadowing-import-from #:cm #:pwd #:cd #:quantize #:at #:now #:tuning #:cycle #:rts?)
  (:shadowing-import-from #:ou #:range)
  (:shadowing-import-from #:cl-plot #:plot)
  (:use #:incudine
        #:incudine.util
        #:cl-midictl
        #:of-incudine-dsps
        #:incudine-bufs
        #:cl #:cl-refs
        #:cl-sfz
        #:clog-dsp-widgets
        #:ats-cuda
        #:ats-cuda-display
        #:clog-midi-controller
        #:cm
        ))
