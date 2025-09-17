;;; 
;;; package.lisp
;;;
;;; Infrastructure dealing with mobile accelerometers as controllers.
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

(in-package :cl-user)

(defpackage #:clamps-sensors
  (:use #:cl #:cl-refs #:clog-dsp-widgets)
  (:export #:make-sensor
           #:make-sensor-data
           #:sensor-id
           #:sensor-path
           #:sensor-data
           #:sensor-unwatch
           #:sensor-oa #:sensor-ob #:sensor-og
           #:sensor-x #:sensor-y #:sensor-z
           #:sensor-gx #:sensor-gy #:sensor-gz
           #:sensor-gyrox #:sensor-gyroy #:sensor-gyroz
           #:sensor-deltag
           #:sensor-trigger-threshold #:sensor-trigger-timeout
           #:sensor-trigger #:sensor-trigger-active
           #:add-sensor #:remove-sensor #:find-sensor #:list-sensors
           #:with-sensor-add-watch
           #:sensor-trig-active
           #:sensor-trig-threshold
           #:sensor-trig-timeout
           #:sensor-add-trigger-fn
           ))
