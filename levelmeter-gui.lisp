;;; 
;;; levelmeter-gui.lisp
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

(defclass levelmeter ()
  ((num :initform 2 :initarg :num :accessor num)
   (refs :initform nil :initarg :refs :accessor refs)
   (id :initarg :id :accessor id)
   (nodes :accessor nodes)
   (node-group :initform 300 :initarg :node-group :accessor node-group)
   (audio-bus :initform 0 :initarg :audio-bus :accessor audio-bus)))

(defmethod initialize-instance :after ((instance levelmeter) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (refs num node-group audio-bus) instance
    (unless refs
      (setf refs (make-array num
                             :initial-contents
                             (loop repeat num collect (make-ref 0.0d0)))))
    (meters-dsp :freq 10 :refs refs :audio-bus audio-bus :group node-group)))

(defun levelmeter-gui (id gui-parent &key (group 300) refs (num 1))
  (let ((dsp
          (or (find-dsp id)
              (add-dsp 'levelmeter :id id :node-group group :refs refs :num num))))
    (with-slots (refs) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black" :background "#666" :height "16em" :width ,(format nil "~aem" (* 2 num))))))
        (dotimes (idx num)
          (create-o-vumeter
           gui-container
           (bind-ref-to-attr (aref refs idx) "db-value")
           :mapping :pd
           :css '(:height "90%" :width "0.5em" :min-width "0.1em" :margin "0.1em 0.25em" :border "0.1em solid black" :background "#222")))))))

#|

(dump (node 0))
*in-refs*
*out-refs*


(incudine::sin-test 411 0.05 :id 101 :head 200)

|#
