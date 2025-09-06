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

(add-sensor :sensor1)

*my-sensor*
;;; check if it is registered.

;;; (defparameter clamps-sensors::*my-sensor* nil)

(set-val (sensor-data (find-sensor :sensor1))
         (make-sensor-data  (random 2.0) (random 2.0) (random 2.0)
                            (random 2.0) (random 2.0) (random 2.0)
                            (random 2.0) (random 2.0) (random 2.0)
                            (random 2.0) (random 2.0) (random 2.0)))

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

(progn
  (setf (clamps-sensors::sensor-data-oa (get-val (sensor-data (find-sensor :sensor1)))) (random 10.0))
  (set-val (sensor-data (find-sensor :sensor1))
           (get-val (sensor-data (find-sensor :sensor1)))))


(clog:*connections*)
(clog:jquery-execute
 (clog:clog-element :connection-id "0c072538fefd8b6e5ef02eb94e0671f8" :html-id "CLOG69")
 "attr('sensor-data','#S(sensor-data :oa 7.57 :ob 0.00 :og 0.00\\x0A:x 0.00 :y 0.00 :z 0.00\\x0A:gx 0.00 :gy 0.00 :gz 0.00\\x0A:gyrox 0.00 :gyroy 0.00 :gyroz 0.00)\\x0A')"
) #<clog:clog-element connection-id: 0c072538fefd8b6e5ef02eb94e0671f8 html-id: CLOG69>


(clog:jquery-execute clamps-sensors::*my-sensor*
                     "attr('sensor-data','#S(sensor-data :oa 7.57 :ob 1.23 :og 0.30\\x0A:x 3.00 :y 0.20 :z 0.00\\x0A:gx 1.00 :gy 0.00 :gz 0.00\\x0A:gyrox 0.00 :gyroy 0.00 :gyroz 0.00)\\x0A')")



(clog-connection:get-connection-data "0c072538fefd8b6e5ef02eb94e0671f8")

(clog:html-document)

(setf (clog:attribute ))

(list-sensors)

"#S(sensor-data :oa 89.28 :ob 42.11 :og -1.47\n:x 0.32 :y -0.53 :z -1.37\n:gx 0.70 :gy 5.98 :gz 5.23\n:gyrox -1.58 :gyroy -24.97 :gyroz 1.92)"

/[\r\n]+/gm

(bind-refs-to-attrs)

/#S\(sensor-data (.+)\)/g.exec("#S(sensor-data :oa 2.3 :ob 5.12)")[1]


const str = "#S(sensor-data :oa 2.3 :ob 5.12)";

const str2 = myRe1.exec (str)[1];
str2.replaceAll(/:\w+/g, ",") 


str.replaceAll (/ [\n\r]+/, " ").replace(myRe1, "[$1]").replaceAll(/ :\w+/g, ",")

"[0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00]"
