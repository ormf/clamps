;;; 
;;; scope-gui.lisp
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

(defclass scope (cuda-dsp)
  ((scope-type :initform :bus :initarg :type :accessor meter-type
               :type (member :in :out :bus))
   (refs :initform nil :initarg :refs :accessor refs)
   (size :initform nil :initarg :size :accessor size)
   (audio-bus :initform 0 :initarg :audio-bus :accessor audio-bus)))

(defmethod initialize-instance :after ((instance scope) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (refs scope-type num nodes node-group audio-bus) instance
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

(defun bus-scope-gui (id gui-parent &key (group 300) (amp (make-ref 1.0d0)) refs (num 1) (audio-bus 0) (channel-offset 0)
                                                  (create-bus t))
  (let* ((dsp
           (or (find-dsp id)
               (let ((new (add-dsp 'master-bus-levelmeter id :node-group group
                                                          :audio-bus audio-bus :refs refs :num num
                                                          :channel-offset channel-offset
                                                          :create-bus create-bus)))
                 (loop until (bus-node new))
                 (push (watch (lambda () (set-control (bus-node new) :amp (get-val amp)))) (unwatch new))
                 new))))
    (with-slots (refs bus-node) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black" :background "transparent" :height "12em" :width "6em" :margin-left "0.1em" :margin-right "0.1em"))))
        (dotimes (idx num)
          (create-o-vumeter
           gui-container
           (bind-refs-to-attrs (aref refs idx) "db-value")
           :mapping :pd
           :css '(:height "97%" :width "0.25em" :min-width "0.1em" :margin "1.75% 1.75% 1.5% 1.5%" :border "0.1em solid black" :background "#222")))
        (create-o-slider
         gui-container
         (bind-refs-to-attrs amp "value")
         :css '(:height "97%" :width "0.5em" :min-width "0.1em" :margin "1.75% 1.75% 1.5% 1.5%" :border "0.1em solid black" :background "#999"))))))

#|



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
  (setf (style body :display) "flex")
  (levelmeter-gui :lm-in body :type :in :group 100 :num 8)
  (levelmeter-gui :lm-out body :type :out :group 300 :num 8))

(set-on-new-window #'meters-window :path "/meters" :boot-file "/start.html")

(defparameter *open-meter-guis* (make-ref 0))
|#

#|

(dump (node 0))
*in-refs*
*out-refs*
(setup-meters)
(find-dsp :lm-in)
(remove-dsp :lm-in)
(remove-dsp :lm-in)
(node-free-all)

(incudine::sin-test 411 0.05 :id 101 :head 200)

|#
