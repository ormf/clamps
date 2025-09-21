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

;;; wrappers for the above have been defined (check sensor.lisp):

(find-sensor :sensor1)

(sensor-trig-active :sensor1)  ; => t
(setf (sensor-trig-active :sensor1) nil) ; => nil
(sensor-trig-active :sensor1) ; => nil
(setf (sensor-trig-active :sensor1) t) ; => t

(sensor-trig-threshold :sensor1) ; => 0.1
(setf (sensor-trig-threshold :sensor1) 100)  ; => 0.12

(sensor-trig-timeout :sensor1) ; => 200
(setf (sensor-trig-timeout :sensor1) 500) ; => 500

(sensor-add-trigger-fn :sensor1
 (lambda () (imsg :warn "triggered")))









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

;;; speedlim-watch:

(defparameter *my-value* (make-ref 1.0))


(progn
  (unwatch-all *unwatch*)
  (push
   (speedlim-watch
    1
    (lambda ()
      (imsg :warn "value: ~a" (get-val *my-value*))))
   *unwatch*))

(loop
  for i below 4000
  do (at (+ (now) (* i 0.01))
         (lambda () (set-val *my-value* (random 1.0)))))

;;; reset timeout
(funcall (first *unwatch*) 0.5)

1.trigger triggers at > 0.1 delta-g
2.triggers triggers at > 0.2 delta-g

delta-g = 2.1 

(loop
  for i below 4000
  do (at (+ (now) (* i 0.01))
         (lambda () (set-val *my-value* (random 1.0)))))

(setf (logger-level) :info)

(progn
  (unwatch-all *unwatch*)
  (push (watch
         (lambda () (imsg :warn "delta-g: ~,4f"
                     (slot-value
                      (get-val (sensor-data (find-sensor :sensor1)))
                      'clamps-sensors::deltag))))
        *unwatch*))

(defparameter *my-delta-g* (make-ref 1.0))

(progn
  (unwatch-all *unwatch*)
  (push (watch
         (lambda () (set-val *my-delta-g*
                        (slot-value
                         (get-val (sensor-data (find-sensor :sensor1)))
                         'clamps-sensors::deltag))))
        *unwatch*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trigger mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *unwatch* nil)
(defparameter *active-triggers* nil)
(defparameter *my-trigger-fns* (make-hash-table))

(defun make-trigger-fn (threshold timeout fn)
  (let ((pending))
    (lambda (delta-g)
      (when (> delta-g threshold)
        (unless pending
          (funcall fn delta-g)
          (setf pending t)
          (at (+ (now) timeout) (lambda () (setf pending nil))))))))

(defun add-trigger (key)
  (pushnew (gethash key *my-trigger-fns*) *active-triggers*))

(defun remove-trigger (key)
  (setf *active-triggers*
        (delete (gethash key *my-trigger-fns*) *active-triggers*)))

(defun list-active-triggers ()
  (loop for fn in *active-triggers*
    collect (gethash fn *my-trigger-fns*)))


;;; add trigger functions to the hash-table:

(progn
  (setf (gethash :trigger1 *my-trigger-fns*)
        (make-trigger-fn
         0.1 1
         (lambda (delta-g)
           (apply #'preset-play
                  `(:preset 1
                    :dtimefn (n-exp (/ (clamps:sensor-oa) 360) 0.01 0.3)
                    :g1 ,cl-poolplayer::*pool2*
                    :transpfn (n-lin (/ (clamps:sensor-oa) 360) -10 10) 
                    :dur ,(random 6)))
           (imsg :warn "trigger1: ~a" delta-g)))
        (gethash (gethash :trigger1 *my-trigger-fns*) *my-trigger-fns*)
        :trigger1        
        (gethash :trigger2 *my-trigger-fns*)
        (make-trigger-fn
         0.15 2.3
         (lambda (delta-g) (imsg :warn "trigger2: ~a" delta-g)))
        (gethash (gethash :trigger2 *my-trigger-fns*) *my-trigger-fns*)
        :trigger2))

*my-trigger-fns*

;;; activate the watch:

(progn
  (unwatch-all *unwatch*)
  (push (watch
         (lambda () (let ((delta-g
                       (slot-value
                        (get-val (sensor-data (find-sensor :sensor1)))
                        'clamps-sensors::deltag)))
                 (dolist (fn *active-triggers*)
                   (funcall fn delta-g)))))
        *unwatch*))

;;; add/remove trigger functions:

(sensor-x)
(add-trigger :trigger1)
(add-trigger :trigger2)

(remove-trigger :trigger1)
(remove-trigger :trigger2)

(setf *active-triggers* nil)

(list-active-triggers)

#|

(defparameter *test-tf* nil)

(setf *test-tf*
      (make-trigger-fn
       0.2 2
       (lambda (delta-g) (imsg :warn "triggered: ~a" delta-g))))

(funcall *test-tf* 0.3)
|#

Let over Lambda by Doug Hoyte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; example of a plus object in pd.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-plus (arg)
  (let ((arg arg))
    (lambda (msg &optional value)
      (case msg
        (:float-left
         (+ value arg))
        (:float-right
         (setf arg value)
         (values))))))

(defparameter *my-plus1*
  (make-plus 3))

(funcall *my-plus1* :float-left 2)

(funcall *my-plus1* :float-right 200)

(funcall *my-plus1* :float-left 2)


https://www.atlassian.com/git/tutorials/git-lfs

mu4e

(setf (ccin 1) 0.2)

(defparameter *gui-amp* (make-ref 0))



(watch (lambda () (set-val (amp (find-controller :amp1)) (ccin 1))))




(apply #'preset-play
       `(:preset 0
         :dtimefn (n-exp (/ (clamps:sensor-oa) 360) 0.01 0.3)
         :g1 ,cl-poolplayer::*pool2*
         :transpfn (n-lin (/ (clamps:sensor-oa) 360) -10 10) 
         :dur ,(random 6)))
