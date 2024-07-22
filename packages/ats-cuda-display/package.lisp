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

(defpackage #:ats-cuda-display
  (:use :cl :incudine :cudere-clm :sb-loop :ieee-floats :alexandria
;;;        :ats-cuda
   :clog-dsp-widgets :cl-refs :clog :clog-dsp-widgets
;;;        :de.finnendahl.binary-data :de.finnendahl.binary-data.common-datatypes
        )
  (:shadowing-import-from :clog
   :run :rotate)
  (:shadowing-import-from :ats-cuda
   #:ats-sound-frames #:ats-sound-name #:ats-sound-sampling-rate #:ats-sound-frame-size
   #:ats-sound-window-size #:ats-sound-partials #:ats-sound-frames #:ats-sound-bands
   #:ats-sound-optimized #:ats-sound-ampmax #:ats-sound-frqmax #:ats-sound-frq-av
   #:ats-sound-amp-av #:ats-sound-dur #:ats-sound-time #:ats-sound-frq #:ats-sound-amp
   #:ats-sound-pha #:ats-sound-energy #:ats-sound-band-energy
   #:ats->svg)
  (:shadowing-import-from :incudine
   :play :scale-envelope :normalize-envelope)
  (:shadowing-import-from :incudine.util
   :sample)
  (:shadowing-import-from :cudere-clm
   :*debug*)
  (:export
   #:ats-player-node-id #:ats-sound #:ats-fmod #:ats-amod #:ats-bw #:ats-x
   #:ats-shift-x #:ats-width #:ats-idx #:ats-data #:ats-crosshairs #:ats-res-balance
   #:ats-mousepos #:ats-scale #:ats-play #:data-watch #:play-watch #:pos-watch
   #:ats->browser #:ats-display #:ats-display-init #:restore-tables))
