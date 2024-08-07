;;; 
;;; load-packages.lisp
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

(in-package :cl-user)

(pushnew (format nil "~a/" (namestring (asdf:system-relative-pathname :clamps "packages/")))
      asdf:*central-registry*)

;;; (push (asdf:system-relative-pathname :clamps "packages/fudi/") asdf:*central-registry*)

(dolist (system '(:cuda-usocket-osc
                  :ats-cuda-display
                  :fudi
                  :of-incudine-dsps
                  :incudine-bufs
                  :orm-utils
                  :cl-plot
                  :incudine-plot
                  :cl-refs
                  :cl-sfz
                  :cl-poolplayer
                  :cm-svg
                  :cm-svg.rts
                  :cm-sfz
                  :cm-poolevt
                  :cm-poolplayer
                  :cm-fomus
                  :cm-incudine
                  :cm-utils
                  :clog-dsp-widgets
                  :clog-cuda
                  :cl-midictl
                  :clog-midi-controller
                  :cm-all))
  (asdf:operate 'asdf:load-op system))

(setf asdf:*central-registry*
      (remove (asdf:system-relative-pathname :clamps "packages/") asdf:*central-registry* :test #'equal))

;;; (pop asdf:*central-registry*)
(pop asdf:*central-registry*)
