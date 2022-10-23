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
(unless (find-package :cm-utils) (ql:quickload "cm-utils"))

(in-package :cl-sfz)

(rt-start)

(load-sfz-preset "/home/orm/work/snd/sfz/bassoboe/bassoboe-f.sfz" :bassoboe-f :force t)
(load-sfz-preset "/home/orm/work/snd/sfz/bassoboe/bassoboe-pp.sfz" :bassoboe-pp :force t)

(play-sfz 60.5 0 1 :preset :bassoboe-f)

(defun play-scale (preset start end &key (dtime 0.5) (ampdb -6) (delta 1))
  (let ((curr start))
    (labels ((inner (time)
               (if (<= curr end)
                   (let ((next (+ time dtime)))
                     (play-sfz curr ampdb (* 1.1 dtime) :preset preset)
                     (incf curr delta)
                     (cm:at next #'inner next)))))
      (inner (cm:now))))) 

(cm:now)
(play-scale :bassoboe-pp 45 84 :ampdb -12 :dtime 1)
(play-scale :bassoboe-f 45 84 :ampdb -12 :dtime 1)

(progn
  (load-sfz-preset "/home/orm/work/snd/sfz/Flute-nv/000_Flute-nv.sfz" :flute-nv :force t)
  (load-sfz-preset "/home/orm/work/snd/sfz/sol-flute/Fl-aeol/Fl-aeol.sfz" :flute-aeol :play-fn #'sample-play :force t)
  (load-sfz-preset "/home/orm/work/snd/sfz/sol-flute/Fl-key-cl/Fl-key-cl.sfz" :flute-key-cl :play-fn #'sample-play :force t)
  (load-sfz-preset "/home/orm/work/snd/sfz/sol-flute/Fl-tng-ram/Fl-tng-ram.sfz" :flute-tng-ram :play-fn #'sample-play :force t)
  (load-sfz-preset "/home/orm/work/snd/sfz/sol-flute/Fl-pizz/Fl-pizz.sfz" :flute-pizz :play-fn #'sample-play :force t))

(untrace)
(incudine::sfz->lsample)

(parse-sfz "/home/orm/work/snd/sfz/sol-flute/Fl-aeol/Fl-aeol.sfz")
(parse-sfz "/home/orm/work/snd/sfz/sol-flute/Fl-key-cl/Fl-key-cl.sfz")
(parse-sfz "/home/orm/work/snd/sfz/sol-flute/Fl-tng-ram/Fl-tng-ram.sfz")
(parse-sfz "/home/orm/work/snd/sfz/sol-flute/Fl-pizz/Fl-pizz.sfz")

(untrace)

(elt (gethash :flute-aeol *sf-tables*) 51)

(incudine::sfz->lsample)

(play-sample 60.5 0 1 :preset :flute-nv)
(play-sample 65 0 10 :preset :flute-aeol)

(play-sample 60 6 0.2 :preset :flute-key-cl)

(play-sample 60 6 10 :preset :flute-tng-ram)

(loop
  for keynum from 48 to 83 by 0.1
  for time from 0 by 0.031
  do (incudine::at (+ (incudine::now) (* incudine::*sample-rate* time))
                   #'play-sample keynum 16 0.2 :preset :flute-key-cl))



(loop
  for keynum from 48 to 83 by 0.1
  for time from 0 by 0.031
  do (incudine::at (+ (incudine::now) (* incudine::*sample-rate* time))
                   #'sample-play (incudine::lsample-buffer (first (aref (gethash :flute-tng-ram *sf-tables*) 127))) 0.02 6 (ct->fv (- keynum 48))
                   0.5 0.05))

(loop
  for keynum from 48 to 83 by 0.1
  for time from 0 by 0.031
  do (incudine::at (+ (incudine::now) (* incudine::*sample-rate* time))
                   #'sample-play (incudine::lsample-buffer (first (aref (gethash :flute-tng-ram *sf-tables*) 56))) 0.02 6 (ct->fv (- keynum 48))
                   0.5 0.05))

(sample-play (incudine::lsample-buffer (first (aref (gethash :flute-tng-ram *sf-tables*) 127))) 0.2 6 0.5)


(load-sfz-preset "/home/orm/work/snd/sfz/sol-flute/Fl-tng-ram/Fl-tng-ram.sfz" :flute-tng-ram :force t)
(play-sample 48 18 0.2 :preset :flute-pizz)



(ct->fv (- 48 (incudine::lsample-keynum (random-elem (aref (gethash :flute-pizz *sf-tables*) (round 48))))))

(elt (gethash :flute-pizz *sf-tables*) 48)
(->lsample)


(sprout
 (process
   for keynum from 36 to 72 do
   
   wait 0.2))

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

(play-chord 34)

(cm::pick 0.5 0.25)

(defparameter *playing* t)
(defparameter *basenote* 25)
(defparameter *p-range* '(19 20))
(defparameter *basenote* 21)
(defparameter *deviation* 0.3)
(defparameter *stretch* 24/24)

(defun play-chord ()
  (let ((next (+ (now) (* incudine::*sample-rate* (+ 0.1 (random 0.5))))))
    (play-lsample (ftom (* (mtof *basenote*)
                           (expt
                            (* (expt 2 (/ (if (zerop *deviation*) 0
                                              (- (random (* 2 *deviation*)) *deviation*))
                                          12))
                               (+ (first *p-range*)
                                  (random (- (second *p-range*) (first *p-range*) -1))))
                            *stretch*)))
                  (+ -20 (random 12))
                  (+ 5 (random 5))
                  :pan (random 1.0))
    (at next #'play-chord)))


(play-lsample 60 0 4 :pan (random 1.0))

(defun play-chord ()
  (let ((next (+ (now) (* incudine::*sample-rate* (cm::pick 0.5 1 2)))))
    (play-lsample (+ 60 (random 4.3)) 0 0.4 :pan (random 1.0))
    (at next #'play-chord)))

(defun play-chord ())

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
#|
;; examples: 

 (cl-sfz:play-lsample (+ 50 (random 30)) 3 0.5)


 (loop
   for x from 1 to 200
   for time = (now) then (+ time (* *sample-rate* (random 0.05)))
   for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
   do (at time #'cl-sfz:play-lsample (+ 70 (random 20.0)) 2 amp))

bouncing to disk:

 (bounce-to-disk ("/tmp/test.wav" :pad 2)
  (loop
     for x from 1 to 200
     for time = (now) then (+ time (* *sample-rate* (random 0.05)))
     for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
     do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp)))
|#
