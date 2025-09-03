;;; 
;;; sensors.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :clamps-sensors)

(defparameter *sensors* nil)

(defstruct sensor id path data unwatch)

(defstruct (sensor-data (:constructor make-sensor-data (&optional oa ob og x y z gx gy gz gyrox gyroy gyroz))
                        (:print-object))
  (oa 0) (ob 0) (og 0)
  (x 0) (y 0) (z 0)
  (gx 0) (gy 0) (gz 0)
  (gyrox 0) (gyroy 0)(gyroz 0))

(defmethod print-object ((data sensor-data) stream)
  (format stream "#S(sensor-data :oa ~,2f :ob ~,2f :og ~,2f~&:x ~,2f :y ~,2f :z ~,2f~&:gx ~,2f :gy ~,2f :gz ~,2f~&:gyrox ~,2f :gyroy ~,2f :gyroz ~,2f)~%"
          (sensor-data-oa data)(sensor-data-ob data)(sensor-data-og data)
          (sensor-data-x data)(sensor-data-y data)(sensor-data-z data)
          (sensor-data-gx data)(sensor-data-gy data)(sensor-data-gz data)
          (sensor-data-gyrox data)(sensor-data-gyroy data)(sensor-data-gyroz data)))

(defun clog-dsp-widgets:sensor-data-reader-fn (&rest data)
  (apply #'make-sensor-data data))

(defun add-sensor (id)
  (let* ((sensor-data (make-ref (make-sensor-data)))
         (path (format nil "/~a" id)))
    (labels ((new-sensor-window (body)
               "On-new-window handler."
               (setf (clog:title (clog:html-document body)) "Sensors Test")
               (let ((collection (create-collection body "1/1")))
                 (create-o-sensors collection (bind-refs-to-attrs
                                               sensor-data "sensor-data")
                                   :interval 50 :css '(:font-size "3em")
                                   :orientation t
                                   :xyz nil
                                   :gxyz nil
                                   :gyro nil))))
      (setf (getf *sensors* id)
            (make-sensor :id id
                         :path path
                         :data sensor-data))
      (format t "adding sensor ~S~%" id)
      (clog:set-on-new-window #'new-sensor-window :path path))))

(defun find-sensor (id)
  (getf *sensors* id))

(defun list-sensors ()
  (sort
   (loop for id in *sensors* by #'cddr
         collect id)
   (lambda (x y) (string< (symbol-name x)(symbol-name y)))))

(defmacro with-sensor-add-watch ((var id) &body body)
  `(let ((,var (find-sensor ,id)))
     (push (watch (lambda () ,@body)) (sensor-unwatch ,var))))

(defun remove-sensor (id)
  (let ((sensor (getf *sensors* id)))
    (if sensor
        (progn
          (format t " ~&removing sensor ~S~%" id)
          (clog:set-on-new-window #'identity :path (sensor-path sensor) :boot-file nil)
          (unwatch-all (sensor-unwatch sensor))
          (remf *sensors* id)
          nil)
        (warn "can't remove sensor ~S: Sensor isn't registered." id))))
