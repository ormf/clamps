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
  ((id :initarg :id :reader dsp-id :documentation "Accessor for the id slot of dsp /object/. Read-only.
@See-also
cuda-dsp")
   (nodes :initform '() :accessor dsp-nodes
          :documentation "Accessor for the nodes slot of dsp /object/.
@See-also
cuda-dsp")
   (node-group :initform 300 :initarg :node-group :accessor node-group
               :documentation "Accessor for the node-group slot of dsp /object/.
@See-also
cuda-dsp")
   (unwatch :initform nil :initarg :unwatch :accessor unwatch
            :documentation "Accessor for the unwatch slot of dsp /object/.
@See-also
cuda-dsp
clamps:cl-refs"))
  ( :documentation "Superclass for dsp objects of the dsp infrastructure
of /clog-dsp-widgets/ in Clamps. An object derived from this type will
be created using the <<add-dsp>> function and its slots automatically
filled on initialization.

cuda-dsp implements the following slots with accessor methods of the
same name (if not indicated otherwise) and initargs being the keywords
of the slot symbol:

=id= -- The id used in the clamps dsp infrastructure. Accessor is <<dsp-id>>.

=nodes= -- The active Incudine nodes of a running instance. Accessor is <<dsp-nodes>>

=node-group= -- The Incudine group of a running instance. Defaults to 300.

=unwatch= -- List of unwatch functions used by the instance.

@Note
Except for the /unwatch/ slot, the user normally will not be dealing
with the slots of a cuda-dsp instance directly except for inspection
purposes. The slots are documented here mainly for clarity and
insight.

@See-also
clamps:clog-dsp-widgets
add-dsp
"))

(defmethod initialize-instance :after ((instance cuda-dsp) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (id) instance
    (incudine.util:msg :info "~&adding dsp: ~S~%" id)))

(defgeneric cuda-dsp-cleanup (instance)
  (:method ((instance cuda-dsp))
    (with-slots (nodes unwatch) instance
      (mapc #'free nodes)
      (mapc #'funcall unwatch))))
