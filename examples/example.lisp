;;; 
;;; example.lisp
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

(incudine:setup-io)

(in-package :clog-dsp-widgets)

(progn
  (node-free-all)
  (setup-io)
  (add-dsp 'levelmeter :id :lm-in :node-group 100 :refs *in-refs*)
  (add-dsp 'levelmeter :id :lm-out :audio-bus 8 :node-group 300 :refs *out-refs*))

(defparameter *x* (make-ref 0))

(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Levelmeter Test")
;;;  (nanoktl2-preset-gui :nk2 body)
;;;  (faderfox-gui :ff01 body)
;;;  (create-o-slider body (bind-ref-to-attr *x* "value") :css '(:width "10em" :height "80em"))
  (levelmeter-gui :lm-in body :group 100 :refs *in-refs* :num 8)
  (levelmeter-gui :lm-out body :group 300 :refs *out-refs* :num 8))

(start)



