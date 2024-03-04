;;; 
;;; gui-base-class.lisp
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

(defclass cuda-dsp ()
  ((id :initarg :id :accessor dsp-id)
   (nodes :initform '() :accessor dsp-nodes)
   (node-group :initform 300 :initarg :node-group :accessor node-group)))

(defmethod initialize-instance :after ((instance cuda-dsp) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (id) instance
    (incudine.util:msg :info "~&adding dsp: ~S~%" id)))
