;;; package.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2017 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(defpackage #:cm-incudine
  (:use #:cl #:cm))

(export '(*rts-out* incudine-stream
          samps->time time->samps secs->samps samps->secs at amp->velo
          jackmidi-input-stream jackmidi-output-stream osc-output-stream fudi-output-stream
          midi-out ctl-out note-on note-off pitch-bend pgm-change midi-note midi-write-message
          incudine-ensure-microtuning write-event rts-enqueue
          midi-open-default midi-close-default

          incudine-stream incudine-input incudine-output
;;; rt-stream-latency incudine-inbuf-size incudine-outbuf-size
          *fudi.in* *fudi-out* fudi fudi-open-default fudi-open fudi-close-default
          send-fudi
          )
        'cm)
