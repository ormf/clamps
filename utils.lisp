;;; 
;;; utils.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-midictl)

(defmacro toggle-slot (slot)
  `(setf (val ,slot)
         (if (zerop (val ,slot))
             127 0)))

(defun buchla-scale (curr old target &key (max 127))
  "scale the target fader by interpolating using the curr and old values
of the source fader."
  (float
   (cond
     ((= old target) curr)
     ((= curr old) target)
     ((< curr old)
      (* (- 1 (/ (- old curr) old)) target))
     (t (- max (* (- 1 (/ (- curr old) (- max old))) (- max target)))))
   1.0))

