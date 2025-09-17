;;; 
;;; sensors-example.lisp
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

;;; first load clamps

(ql:quickload :clamps)

(in-package :clamps)

;;; start clamps

(clamps)

;;; register a sensor, which will automatically add a webpage
;;; responder with the URL https://<ip>:54619/sensor1 for connecting
;;; from a mobile.

(progn
  (remove-sensor :sensor1)
  (add-sensor :sensor1))

;;; check if the sensor is registered:

(find-sensor :sensor1)

;;; setting properties of the sensor:

(set-val (sensor-trigger-timeout (find-sensor :sensor1)) 200)
(set-val (sensor-trigger-threshold (find-sensor :sensor1)) 0.1)

;;; activate trigger
(set-val (sensor-trigger-active (find-sensor :sensor1)) t)

;;; deactivate trigger
(set-val (sensor-trigger-active (find-sensor :sensor1)) nil)

;;; check if it is registered.

(find-sensor :sensor1)

;;; defining a function to be invoked on trigger:

(add-trigger-fn
 (sensor-trigger (find-sensor :sensor1))
 (lambda () (imsg :warn "triggered")))

;;; wrappers for the above:



;;; attach behaviour to value changes received from the mobile (here
;;; we just print out the sensor-data values in the repl whenever they
;;; are changed).

(with-sensor-add-watch (sensor :sensor1)
  (format t "~&~a%" (get-val (sensor-data sensor))))

;;; load the page on the mobile, and press "Start Demo". The motion
;;; params should be monitored in the repl.

;;; ...

;;; Remove the webpage, the sensor and its watch function:

(remove-sensor :sensor1)

;;; Explanation of with-sensor-add-watch
;;;
;;; The first argument of with-sensor-add-watch binds a symbol to a
;;; sensor referenced by its id keyword in the lexical scope of the
;;; body of wih-sensor-add-watch.
;;;
;;; The body of with-sensor-add-watch will be the body of a watch
;;; function.
;;;
;;; The with-sensor-add-watch macro pushes the result of the watch
;;; function to the unwatch slot of the sensor struct so that all
;;; watch functions can be automatically removed on removing the
;;; sensor.

;;;
;;; the sensor-data slot of sensor contains a ref-cell with a struct
;;; containing all sensor values as content. Here we simply print out
;;; the struct whenever a value changes.
;;;

(defparameter *unwatch* nil)

(progn
  (unwatch-all *unwatch*)
  (push (watch (lambda () (format t "~a" (get-val (sensor-data (find-sensor :sensor1))))))
        *unwatch*))

(let ((sensor (find-sensor :sensor1)))
  (push (lambda () (format t "~&~a%" (get-val (sensor-data sensor))))
        (sensor-unwatch sensor)))
