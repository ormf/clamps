;;; 
;;; envelope.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-poolplayer)

#| 

An envelope is a special data-structure defining an asr envelope:

   ______|______
  /      |      \ 
 /       |       \
/        |        \

the two fields suswidth and pan define the envelope:

suswidth: length of the sustain phase in relation to the total
          length (the value is between 0 and 1). If it is zero, then
          there will be no sustain phase so the attack is immediately
          followed by the release. If it is 1 there will be no attack
          and no release.

pan: the position of the center of the suswidth phase (also between 0
     and 1). The value therefore specifies the proportion between
     attack and release. If the Pan is 0 then there will be no attack,
     if it is 1 there will be no release.

The env types :pitch :dtime and :amp differ in the way, the y-values
are determined:

|#

#|
(let ((env (sv (aref *presets* 0) :amp-env)))
  (sv env :delta 1 :pan 0.1 :suswidth 0.2)
  env)

(set-env ((aref *presets* 0) :amp-env)
  :delta 0.1 :pan 0.3 :suswidth 0.5)
|#

(defstruct env
  (start 0)
  (delta 0)
  (attack 0)
  (release 0)
  (type :lin))

(defun get-env-y (x env)
  "given a normalized x value, return the y value of the
env. Depending on the envelope type, the interpolation is
linear (0..delta) or exponential (1..delta)."
  (with-slots (start attack delta release type) env
    (let* ((release-start (- 1 release)))
      (cond
        ((< x attack)
         (case type
           (:lin (if (zerop attack)
                     (+ start delta)
                     (+ start (* (/ x attack) delta))))
           (:exp (if (zerop attack)
                     (* start delta)
                     (* start (expt delta (/ x attack)))))))
        ((>= x release-start)
         (case type
           (:lin (if (zerop release)
                     (+ start delta)
                     (+ start (* (- 1 (/ (- x release-start) release)) delta))))
           (:exp (if (zerop release)
                     (* start delta)
                     (* start (expt delta (- 1 (/ (- x release-start) release))))))))
        (:else (case type
                 (:lin (+ start delta))
                 (:exp (* start delta))))))))

(defun rand-dev (dev)
  (if (zerop dev) 0 (- dev (* (signum dev) (random (* 2.0 (abs dev)))))))

(defun rand-pos (dev)
  (if (zerop dev) 0 (* (signum dev) (random  (float (abs dev))))))

(defun rand-get-env-y (x env)
  "given a normalized x value, return a random value of the env
between with a max-value according to the env. Depending on the envelope type, the
interpolation is linear (0..y) or exponential (1..y)."
  (with-slots (start attack delta release type) env
    (let* ((release-start (- 1 release)))
      (cond
        ((< x attack)
         (case type
           (:lin (if (zerop attack)
                     (rand-pos (+ start delta))
                     (rand-pos (+ start (* (/ x attack) delta)))))
           (:exp (if (zerop attack)
                     (expt (* start delta) (random 1.0))
                     (expt (+ start (* (/ x attack) delta)) (random 1.0))))))
        ((>= x release-start)
         (case type
           (:lin (if (zerop release)
                     (rand-pos (+ start delta))
                     (rand-pos (+ start (* (- 1 (/ (- x release-start) release)) delta)))))
           (:exp (if (zerop release)
                     (expt (* start delta) (random 1.0))
                     (expt (+ start (* (- 1 (/ (- x release-start) release)) delta)) (random 1.0))))))
        (:else (case type
                 (:lin (rand-pos (+ start delta)))
                 (:exp (expt (* start delta) (random 1.0)))))))))


;;; (rand-get-env-y 0 (make-env :start -40 :delta 0 :attack 1 :release 0 :type :lin))

(defun rand-dev-get-env-y (x env)
  "given a normalized x value, return a random value of the env
between with a max-value according to the env. Depending on the envelope type, the
interpolation is linear (-y..y) or exponential (1/y..y)."
  (with-slots (start attack delta release type) env
    (let* ((release-start (- 1 release)))
      (cond
        ((< x attack)
         (case type
           (:lin (if (zerop attack)
                     (rand-dev (+ start delta))
                     (rand-dev (+ start (* (/ x attack) delta)))))
           (:exp (if (zerop attack)
                     (expt (* start delta) (- 1 (random 2.0)))
                     (expt (+ start (* (/ x attack) delta)) (- 1 (random 2.0)))))))
        ((>= x release-start)
         (case type
           (:lin (if (zerop release)
                     (rand-dev (+ start delta))
                     (rand-dev (+ start (* (- 1 (/ (- x release-start) release)) delta)))))
           (:exp (if (zerop release)
                     (expt (* start delta) (- 1 (random 2.0)))
                     (expt (+ start (* (- 1 (/ (- x release-start) release)) delta)) (- 1 (random 2.0)))))))
        (:else (case type
                 (:lin (rand-dev (+ start delta)))
                 (:exp (expt (* start delta) (- 1 (random 2.0))))))))))

#|

(rand-get-env-y 1 (make-env :start 1 :delta 1 :attack 0 :release 0 :type :exp))
(get-env-y 1 (make-env :start 1 :delta 1 :attack 0 :release 0 :type :exp))

(defun get-env-val (time env &rest args)
  "get the envelope-value of env with mormalized time."
  (case (env-type env)
    (:lin 1)
    (:exp 2)))

(set-env ((aref *presets* 0) :dtime-env) :suswidth 0.2 :pan 0.23 :delta 6)
(loop for x below 11 collect (list (/ x 10.0) (get-env-y (/ x 10) (sv (aref *presets* 0) :dtime-env))))

(set-env ((aref *presets* 0) :amp-env) :suswidth 0.2 :pan 0.3 :delta 6)
(loop for x below 11 collect (list (/ x 10.0) (get-env-y (/ x 10) (sv (aref *presets* 0) :amp-env))))

|#
