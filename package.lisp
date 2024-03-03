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

(export '(cuda-dsp dsp-id levelmeter num-meters refs dsp-nodes node-group audio-bus levelmeter-gui
          levelmeter-full-gui *in-refs* *out-refs*)
        'clog-dsp-widgets)
