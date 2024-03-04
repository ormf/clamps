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

(defclass levelmeter (cuda-dsp)
  ((meter-type :initform :bus :initarg :type :accessor meter-type
               :type (member :in :out :bus))
   (num :initform 2 :initarg :num :accessor num-meters)
   (refs :initform nil :initarg :refs :accessor refs)
   (audio-bus :initform 0 :initarg :audio-bus :accessor audio-bus)))

(defmethod initialize-instance :after ((instance levelmeter) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (refs meter-type num nodes node-group audio-bus) instance
    (unless refs
      (setf refs (make-array num
                             :initial-contents
                             (loop repeat num collect (make-ref 0.0d0)))))
;;;    (incudine.util:msg :warn "heyho")
    (case meter-type
      (:bus
       (meters-dsp :id-callback (lambda (id) (push id nodes)) :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group))
      (:in
       (inmeters-dsp :id-callback (lambda (id) (push id nodes)) :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group))
      (:out
       (outmeters-dsp :id-callback (lambda (id) (push id nodes)) :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group)))))


(defun levelmeter-gui (id gui-parent &key (group 300) (type :bus) refs (num 1) (audio-bus 0))
  (check-type type (member :bus :in :out))
  (let ((dsp
          (or (find-dsp id)
              (add-dsp 'levelmeter :id id :type type :node-group group
                                   :audio-bus audio-bus :refs refs :num num))))
    (with-slots (refs) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black" :background "#666" :height "16em" :width ,(format nil "~aem" (* 2 num))))))
        (dotimes (idx num)
          (create-o-vumeter
           gui-container
           (bind-refs-to-attrs (aref refs idx) "db-value")
           :mapping :pd
           :css '(:height "90%" :width "0.5em" :min-width "0.1em" :margin "0.1em 0.25em" :border "0.1em solid black" :background "#222")))))))

(defun levelmeter-full-gui (id gui-parent &key (group 300) (type :bus) refs (num 1) (audio-bus 0))
  (check-type type (member :bus :in :out))
  (let ((dsp
          (or (find-dsp id)
              (add-dsp 'levelmeter :id id :type type :node-group group
                                   :audio-bus audio-bus :refs refs :num num))))
    (with-slots (refs) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black" :background "#666" :height "100%" :width "100%"))))
        (dotimes (idx num)
          (create-o-vumeter
           gui-container
           (bind-refs-to-attrs (aref refs idx) "db-value")
           :mapping :pd
           :css `(:height "90%" :width ,(format nil "~a%" (/ 80 num)) :min-width "0.1em" :margin ,(format nil "2% 2%" ) :border "0.1em solid black" :background "#222")))))))


(defparameter *in-refs* (coerce (loop repeat 8 collect (cl-refs:make-ref -100)) 'vector))
(defparameter *out-refs* (coerce (loop repeat 8 collect (cl-refs:make-ref -100)) 'vector))

(defun setup-meters ()
  (dolist (id '(:lm-in :lm-out))
    (remove-dsp id))
  (setup-io)
  (add-dsp 'levelmeter :id :lm-in :type :in :num 8 :node-group 100 :refs *in-refs*)
  (add-dsp 'levelmeter :id :lm-out :type :out :num 8 :node-group 300 :refs *out-refs*)
;;;  (add-dsp 'levelmeter :id :lm-out :type :bus :num 8 :audio-bus 8 :node-group 300 :refs *out-refs*)
  )

(defun meters-window (body)
  "handler for /meters"
  (setf (title (html-document body)) "Meters")
  (levelmeter-gui :lm-in body :type :in :group 100 :num 8)
  (levelmeter-gui :lm-out body :type :out :group 300 :num 8))

(set-on-new-window #'meters-window :path "/meters" :boot-file "/start.html")

(defparameter *open-meter-guis* (make-ref 0))

#|

(dump (node 0))
*in-refs*
*out-refs*


(incudine::sin-test 411 0.05 :id 101 :head 200)

|#
