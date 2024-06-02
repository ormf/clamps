;;; 
;;; dsp-registry.lisp
;;;
;;;
;;; a dsp registry for dsps like buses, levelmeters, etc. which live
;;; in persistent group (different from group 200).
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

(in-package :clog-dsp-widgets)

(defparameter *dsps* (make-hash-table :test #'equal))

(export '(add-dsp remove-dsp find-dsp list-dsps) 'clog-dsp-widgets)

(defun add-dsp (dsp &rest args &key id &allow-other-keys)
  (setf (gethash id *dsps*)
        (apply #'make-instance dsp args)))

(defun remove-dsp (id)
  (let ((dsp (find-dsp id)))
    (when dsp
      (cuda-dsp-cleanup dsp)
      (remhash id *dsps*))))

(defun find-dsp (id)
  (gethash id *dsps*))

(defun list-dsps ()
  (format t "current active-dsps:~%")
  (maphash (lambda (id dsp) dsp (format t "~S~%" id)) *dsps*))
