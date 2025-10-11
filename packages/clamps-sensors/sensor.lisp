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

(defstruct sensor id path
  trigger trigger-active
  trigger-threshold trigger-timeout
  data unwatch)

(defstruct (sensor-data (:constructor make-sensor-data (&optional oa ob og x y z gx gy gz gyrox gyroy gyroz deltag))
                        (:print-object))
  (oa 0) (ob 0) (og 0)
  (x 0) (y 0) (z 0)
  (gx 0) (gy 0) (gz 0)
  (gyrox 0) (gyroy 0)(gyroz 0)
  (deltag 0))

(defmethod print-object ((data sensor-data) stream)
  (format stream "#S(sensor-data :oa ~,2f :ob ~,2f :og ~,2f~&:x ~,2f :y ~,2f :z ~,2f~&:gx ~,2f :gy ~,2f :gz ~,2f~&:gyrox ~,2f :gyroy ~,2f :gyroz ~,2f :deltag ~,2f)~%"
          (sensor-data-oa data) (sensor-data-ob data) (sensor-data-og data)
          (sensor-data-x data) (sensor-data-y data) (sensor-data-z data)
          (sensor-data-gx data) (sensor-data-gy data) (sensor-data-gz data)
          (sensor-data-gyrox data) (sensor-data-gyroy data) (sensor-data-gyroz data)(sensor-data-deltag data)))

(defun clog-dsp-widgets:sensor-data-reader-fn (&rest data)
  (apply #'make-sensor-data data))

(defun clog-dsp-widgets::get-attribute-form (val)
  (typecase val
    (clamps-sensors::sensor-data
     (with-slots (oa ob og x y z gx gy gz gyrox gyroy gyroz deltag) val
       (list oa ob og x y z gx gy gz gyrox gyroy gyroz deltag) ))
    (otherwise val)))

(defun add-sensor (id &rest args)
"Add an Accelerometer sensor with /id/ accessible from a Firefox
Browser on a Mobile device at the URL http://<local-ip>:54619/id to
the sensor registry.

Note that the id in the URL will be written without the colon of the
keyword. An id of ~:sensor1~ will result in the URL
~http://<local-ip>:54619/sensor1~.

@Arguments
id - Keyword denoting the id of the sensor.
:trigger-active - Positive Number denoting if the trigger is active. Defaults to t.
:trigger-threshold - Positive Number denoting the trigger threshold. Defaults to 0.1.
:trigger-timeout - Positive Number denoting the trigger timeout in ms. Defaults to 200.

@See-also
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (let* ((sensor-data (make-ref (make-sensor-data)))
         (trigger (make-bang (lambda ()) 0))
         (trigger-active (make-ref (getf args :trigger-active nil)))
         (trigger-threshold (make-ref (getf args :trigger-threshold 0.1)))
         (trigger-timeout (make-ref (getf args :trigger-timeout 200)))
         (path (format nil "/~a" id)))
    (labels ((new-sensor-window (body)
               "On-new-window handler."
               (setf (clog:title (clog:html-document body)) "Sensors Test")
               (let ((collection (create-collection body "1/1")))
                 (create-o-sensor collection (bind-refs-to-attrs
                                              trigger "sensor-trigger"
                                              sensor-data "sensor-data"
                                              trigger-active "trigger-active"
                                              trigger-timeout "trigger-timeout"
                                              trigger-threshold "trigger-threshold"
                                              )
                                  :interval 50 :css '(:font-size "3em")
                                  :orientation t
                                  :xyz nil
                                  :gxyz nil
                                  :gyro nil
                                  :css '(:font-size "3em")))))
      (setf (getf *sensors* id)
            (make-sensor :id id
                         :path path
                         :trigger trigger
                         :trigger-active trigger-active
                         :trigger-threshold trigger-threshold
                         :trigger-timeout trigger-timeout
                         :data sensor-data))
      (format t "adding sensor ~S~%" id)
      (clog:set-on-new-window #'new-sensor-window :path path)
      nil)))

(defun find-sensor (id)
  "Return the sensor struct of sensor referenced by /id/.

@Arguments
id - Keyword of the id of the sensor.

@See-also
add-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (getf *sensors* id))

(defun list-sensors ()
  "Return a list of the ids of all registered sensors.

@See-also
add-sensor
find-sensor
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (sort
   (loop for id in *sensors* by #'cddr
         collect id)
   (lambda (x y) (string< (symbol-name x)(symbol-name y)))))

(defmacro with-sensor-add-watch ((var id) &body body)
  `(let ((,var (find-sensor ,id)))
     (push (watch (lambda () ,@body)) (sensor-unwatch ,var))))

(defun remove-sensor (id)
  "remove the sensor referenced by /id/ from the sensor registry.

@Arguments
id - Keyword of the id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (let ((sensor (getf *sensors* id)))
    (if sensor
        (progn
          (format t " ~&removing sensor ~S~%" id)
          (clog:set-on-new-window #'identity :path (sensor-path sensor) :boot-file nil)
          (unwatch-all (sensor-unwatch sensor))
          (remf *sensors* id)
          nil)
        (warn "can't remove sensor ~S: Sensor isn't registered." id))))

(defun sensor-oa (&optional (id :sensor1))
  "Accessor for Orientation alpha of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-oa (get-val (sensor-data (find-sensor id)))))

(defun sensor-ob (&optional (id :sensor1))
  "Accessor for Orientation beta of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-ob (get-val (sensor-data (find-sensor id)))))

(defun sensor-og (&optional (id :sensor1))
  "Accessor for Orientation gamma of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-og (get-val (sensor-data (find-sensor id)))))

(defun sensor-x (&optional (id :sensor1))
  "Accessor for Accelerometer x of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-x (get-val (sensor-data (find-sensor id)))))

(defun sensor-y (&optional (id :sensor1))
  "Accessor for Accelerometer y of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-y (get-val (sensor-data (find-sensor id)))))

(defun sensor-z (&optional (id :sensor1))
 "Accessor for Accelerometer z of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-z (get-val (sensor-data (find-sensor id)))))

(defun sensor-gx (&optional (id :sensor1))
 "Accessor for g-weighted Accelerometer x of sensor with /id/. As
the <<sensor-data>> of the sensor is a ref-cell, this function can be
used in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-gx (get-val (sensor-data (find-sensor id)))))

(defun sensor-gy (&optional (id :sensor1))
 "Accessor for g-weighted Accelerometer y of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-gy (get-val (sensor-data (find-sensor id)))))

(defun sensor-gz (&optional (id :sensor1))
  "Accessor for g-weighted Accelerometer z of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-gz (get-val (sensor-data (find-sensor id)))))

(defun sensor-gyrox (&optional (id :sensor1))
 "Accessor for Gyrometer x of sensor with /id/. As the <<sensor-data>>
of the sensor is a ref-cell, this function can be used in a <<watch>>
context.

@Arguments
id - id of sensor.
"
  (sensor-data-gyrox (get-val (sensor-data (find-sensor id)))))

(defun sensor-gyroy (&optional (id :sensor1))
 "Accessor for Gyrometer y of sensor with /id/. As the <<sensor-data>>
of the sensor is a ref-cell, this function can be used in a <<watch>>
context.

@Arguments
id - id of sensor.
"
  (sensor-data-gyroy (get-val (sensor-data (find-sensor id)))))

(defun sensor-gyroz (&optional (id :sensor1))
 "Accessor for Gyrometer z of sensor with /id/. As the <<sensor-data>>
of the sensor is a ref-cell, this function can be used in a <<watch>>
context.

@Arguments
id - id of sensor.
"
  (sensor-data-gyroz (get-val (sensor-data (find-sensor id)))))

(defun sensor-deltag (&optional (id :sensor1))
 "Accessor for Orientation alpha of sensor with /id/. As the
<<sensor-data>> of the sensor is a ref-cell, this function can be used
in a <<watch>> context.

@Arguments
id - id of sensor.
"
  (sensor-data-deltag (get-val (sensor-data (find-sensor id)))))

(defun sensor-trig-timeout (sensor)
  "Return the value of the trigger-timeout ref-cell in
/sensor/. Setf-able.

@Arguments
sensor - id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
"
  (get-val (sensor-trigger-timeout (find-sensor sensor))))

(defun set-sensor-trig-timeout (sensor val)
  (set-val (sensor-trigger-timeout (find-sensor sensor)) val))

(defsetf sensor-trig-timeout set-sensor-trig-timeout)

;;; (sensor-trig-timeout :sensor1)
;;; (setf (sensor-trig-timeout :sensor1) 500)

(defun sensor-trig-threshold (sensor)
  "Return the value of the trigger-threshold ref-cell in
/sensor/. Setf-able.

@Arguments
sensor - id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-timeout
"
  (get-val (sensor-trigger-threshold (find-sensor sensor))))

(defun set-sensor-trig-threshold (sensor val)
  (set-val (sensor-trigger-threshold (find-sensor sensor)) val))

(defsetf sensor-trig-threshold set-sensor-trig-threshold)

;;; (sensor-trig-threshold :sensor1)
;;; (setf (sensor-trig-threshold :sensor1) 0.2)

(defun sensor-trig-active (sensor)
  "Return the value of the trigger-active ref-cell in
/sensor/. Setf-able.

@Arguments
sensor - id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-threshold
sensor-trig-timeout
"
  (get-val (sensor-trigger-active (find-sensor sensor))))

(defun set-sensor-trig-active (sensor val)
  (set-val (sensor-trigger-active (find-sensor sensor)) val))

(defsetf sensor-trig-active set-sensor-trig-active)

;;; (sensor-trig-active :sensor1)
;;; (setf (sensor-trig-active :sensor1) nil)
;;; (setf (sensor-trig-active :sensor1) t)

(defun sensor-add-trigger-fn (id fn)
"Add /fn/ to the sensor referenced by /id/ to be called upon a trigger when moving
the accelerometer.

@Arguments
id - Keyword of the id of the sensor.
fn - Function to call when a trigger occurs.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (add-trigger-fn (sensor-trigger (find-sensor id)) fn))

(defun sensor-remove-all-triggers (id)
"remove all trigger functions from the sensor referenced by /id/.

@Arguments
id - Keyword of the id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  (remove-all-triggers (sensor-trigger (find-sensor id))))

(defun sensor-add-motion-fn (id fn)
"Add /fn/ to the sensor referenced by /id/ to be called upon a motion event from the Mobile.

@Arguments
id - Keyword of the id of the sensor.
fn - Function to call when a trigger occurs.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-motion-fns
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  ;; (let ((sensor (find-sensor id)))
  ;;   (push (watch fn (sensor-unwatch sensor))))
  )

(defun sensor-remove-all-motion-fns (id)
"remove all motion functions from the sensor referenced by /id/.

@Arguments
id - Keyword of the id of the sensor.

@See-also
add-sensor
find-sensor
list-sensors
remove-sensor
sensor-add-trigger-fn
sensor-remove-all-triggers
sensor-trig-active
sensor-trig-threshold
sensor-trig-timeout
"
  ;;; (remove-all-triggers (sensor-trigger (find-sensor id)))

  )
