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
  (:export)
  (:shadowing-import-from #:incudine
                          #:rescale-envelope #:init #:stop
                          #:group #:tempo #:control-value
                          #:timestamp #:responder
;;; play-sample
)

  (:shadowing-import-from #:of-incudine-dsps #:clip)

  (:use #:cl #:cl-user #:cm)
  (:shadow #:*tempo* #:buffer #:buffer-frames
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

  (:shadowing-import-from #:cm #:quantize #:at #:now #:tuning #:cycle)
  (:use #:incudine
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
