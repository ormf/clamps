;;;
;;; package.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl
;;; <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

;;; we don't define an extra package but use clog-dsp-widgets and
;;; import incudine's symbols into it.

(in-package :clog-dsp-widgets)

(use-package :incudine)

(export '(cuda-dsp dsp-id levelmeter num-meters refs dsp-nodes node-group
          audio-bus levelmeter-gui
          master-amp-bus-levelmeter-gui
          master-amp-out-levelmeter-gui master-bus-levelmeter-gui levelmeter-full-gui *in-refs* *out-refs*
          master-amp-meter-bus
          named-amp-bus named-bus
          bus-name num-chans audio-bus amp-node
;;;          bus-amp
          channel-offs
          amp->db-slider db-slider->amp db-slider->db db->db-slider
          find-bus add-bus remove-bus list-buses bus-channel
          add-dsp remove-dsp remove-all-dsps find-dsp list-dsps)
        'clog-dsp-widgets)
