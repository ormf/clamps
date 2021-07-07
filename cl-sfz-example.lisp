;;; 
;;; cl-sfz-example.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(unless (find-package :cl-sfz) (ql:quickload "cl-sfz"))

(in-package :cl-sfz)

(rt-start)

(load-sfz "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz" :flute-nv)

(play-lsample 80 -12 1 :preset :flute-nv)

(defun fv->ct (fv)
  (* 12 (log fv 2)))

(defun mtof (m)
  (* 440 (ct->fv (- m 69))))

;;; (mtof 81)

(defun ftom (f)
  (+ 69 (fv->ct (/ f 440))))

(defun play-chord (basenote)
  (loop
    for x from 1 to 20
    for time = (now) then (+ time (* incudine::*sample-rate* (random 0.5)))
    for amp = (+ -24 (random 6))
    do (at time #'play-lsample (ftom (* (mtof basenote) (+ 1 (random 0.01) (random 20)))) amp 20)))

(defparameter *playing* t)

(defparameter *basenote* 25)
(defparameter *p-range* '(14 16))

(defparameter *p-range* '(1 19))

(defparameter *basenote* 25.1)

(defparameter *deviation* 0)

(defparameter *stretch* 24/24)


(defun play-chord ()
  (if *playing*
      (let ((next (+ (now) (* incudine::*sample-rate* (random 0.4)))))
        (play-lsample (ftom (* (mtof *basenote*)
                               (expt
                                (* (expt 2 (/ (if (zerop *deviation*) 0 (- (random (* 2 *deviation*)) *deviation*)) 12))
                                   (+ (first *p-range*)
                                      (random (- (second *p-range*) (first *p-range*) -1))))
                                *stretch*)))
                      (+ -30 (random 12))
                      (+ 5 (random 5))
                      :pan (random 1.0))
        (at next #'play-chord))))

(play-chord)

(defun play-chord ())


(setf *playing* nil)


    for x from 1 to 20
    for time = (now) then (+ time (* incudine::*sample-rate* (random 0.5)))
    for amp = (+ -24 (random 6))


;;; bouncing to disk:

 (bounce-to-disk ("/tmp/test.wav" :pad 2)
  (loop
     for x from 1 to 200
     for time = (now) then (+ time (* *sample-rate* (random 0.05)))
     for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
     do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp)))
