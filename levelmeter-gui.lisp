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
   (audio-bus :initform 0 :initarg :audio-bus :accessor audio-bus)
   (meter-node :initform nil :initarg :meter-node :accessor meter-node)))

(defmethod initialize-instance :after ((instance levelmeter) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (id refs meter-type num meter-node node-group audio-bus unwatch cleanup) instance
    (unless refs
      (setf refs (make-array num
                             :initial-contents
                             (loop repeat num collect (make-ref 0.0d0)))))
    (incudine.util:msg :warn "adding levelmeter ~S" id)
    (case meter-type
      (:bus
       (meters-dsp :id-callback (lambda (id) (setf meter-node id))
                   :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group))
      (:in
       (inmeters-dsp :id-callback (lambda (id) (setf meter-node id))
                     :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group))
      (:out
       (outmeters-dsp :id-callback (lambda (id) (setf meter-node id))
                      :freq 10 :num num :refs refs :audio-bus audio-bus :group node-group)))))

(defmethod cuda-dsp-cleanup ((instance levelmeter))
  (with-slots (meter-node unwatch) instance
    (free meter-node)
    (mapc #'funcall unwatch)))


;;; Levelmeter with bus initialization and amp control.

(defclass master-amp-bus-levelmeter (levelmeter named-amp-bus)
  ((meter-display :initarg :meter-display :initform :post :accessor meter-display :type (member :pre :post))))

;;; (defparameter *test* (make-instance 'master-amp-bus-levelmeter :id :test))

(dump (node 0))
(dogroup (n (node 300))
  (free n))

(defmethod initialize-instance :after ((instance master-amp-bus-levelmeter) &rest initargs)
  (declare (ignorable initargs))
  (with-slots (meter-display amp-node meter-node) instance    
    (loop until (and amp-node meter-node))
    (move meter-node (if (eq meter-display :pre) :before :after) amp-node)))

(defmethod cuda-dsp-cleanup ((instance master-amp-bus-levelmeter))
  (with-slots (meter-node amp-node nodes unwatch) instance
    (mapc #'free nodes)
    (free meter-node)
    (free amp-node)
    (mapc #'funcall unwatch)))

;;; TODO: master-bus-scope

(defun levelmeter-gui (id gui-parent &key (group 300) (type :bus) refs (num 1) (audio-bus 0))
  (check-type type (member :bus :in :out))
  (let ((dsp
          (or (find-dsp id)
              (add-dsp 'levelmeter :id id :type type :node-group group
                                   :audio-bus audio-bus :refs refs :num num))))
    (with-slots (refs) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black"
                                               :background "#666" :height "100%" :width "46%"
                                               :margin-left "2%" :margin-right "2%"))))
        (dotimes (idx num)
          (create-o-vumeter
           gui-container
           (bind-refs-to-attrs (aref refs idx) "db-value")
           :mapping :pd
           :css '(:height "97%" :width "9%" :min-width "0.1em" :margin "1.75% 1.75% 1.5% 1.5%" :border "0.1em solid black" :background "#222")))))))

(defun master-bus-levelmeter-gui (id gui-parent &key (group 300) (amp (make-ref 1.0d0)) refs (num 1) (audio-bus 0) (channel-offset 0) (create-bus t)
                                                  (bus-name ""))
  (let* ((dsp
           (or (find-dsp id)
               (let ((new (add-dsp 'master-bus-levelmeter :id id :node-group group
                                                          :audio-bus audio-bus :refs refs :num num
                                                          :channel-offset channel-offset
                                                          :create-bus create-bus
                                                          :bus-name bus-name)))
                 (loop until (amp-node new))
                 (push (watch (lambda () (set-control (amp-node new) :amp (get-val amp)))) (unwatch new))
                 new))))
    (with-slots (refs bus-node) dsp
      (let* ((gui-container (create-div gui-parent
                                        :class "levelmeter-panel"
                                        :css `(:display "flex" :border "0.1em solid black" :background "transparent"
                                               :height "12em" :width "6em" :margin-left "0.1em" :margin-right "0.1em"))))
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
  (setf (style body :display) "flex")
  (setf (style body :height) "100vh")
  (levelmeter-gui :lm-in body :type :in :group 100 :num 8)
  (levelmeter-gui :lm-out body :type :out :group 400 :num 8))

(set-on-new-window #'meters-window :path "/meters" :boot-file "/start.html")

(defparameter *open-meter-guis* (make-ref 0))

#|
(levelmeter-gui :lm-in body :type :in :group 100 :num 8)
(remove-dsp :lm-in)

(dump (node 0))
*in-refs*
*out-refs*
(setup-meters)
(find-dsp :lm-in)
(remove-dsp :lm-out)
(node-free-all)

(incudine::sin-test 411 0.05 :id 101 :head 200)

|#
