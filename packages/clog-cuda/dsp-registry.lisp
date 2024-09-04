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

(defun add-dsp (dsp id &rest args)
  "Add a new instance of /dsp/ with id /id/ to the registry, optionally
supplying the dsp creation with initialization arguments /args/.

@Arguments
dsp - The dsp type to add
id - Keyword or Symbol to identify the registered dsp.
args - Optional initialization arguments accepted by the used dsp class.

@See-also
find-dsp
list-dsps
remove-dsp
"
  (setf (gethash id *dsps*)
        (apply #'make-instance dsp args)))

(defun remove-dsp (id)
  "Remove a running Incudine dsp registered with <<add-dsp>>.

@Arguments
id - Keyword or Symbol identifying the dsp.

@See-also
add-dsp
find-dsp
list-dsps
"
  (let ((dsp (find-dsp id)))
    (when dsp
      (cuda-dsp-cleanup dsp)
      (remhash id *dsps*))))

(defun remove-all-dsps ()
  (map nil #'remove-dsp (list-dsps)))

(defun find-dsp (id)
  "Find a running Incudine dsp registered with <<add-dsp>>.

@Arguments
id - Keyword or Symbol identifying the dsp.

@See-also
add-dsp
list-dsps
remove-dsp
"
  (gethash id *dsps*))

(defun list-dsps ()
  "Return all running Incudine dsps registered with <<add-dsp>> in a
list.

@See-also
add-dsp
find-dsp
remove-dsp
"
  (loop
    for key being the hash-keys of *dsps*
    collect key))
