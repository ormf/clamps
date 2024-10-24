;;; 
;;; ex-01.lisp
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

(unless (find-package :cl-poolplayer) (ql:quickload "cl-poolplayer"))

(in-package :cl-poolplayer)

(cd "/home/orm/work/kompositionen/big-orchestra/lisp/big-orchestra")

;;; association of key and sample-directories

(defparameter *sound-type-dirs*
  '(:aitken-chimes "aitken-mallet/chimes"
    :aitken-gsp "aitken-mallet/einzeln-gsp"
    :aitken-mallet "aitken-mallet/einzeln-mallet"
    :atoui-feder "atoui/feder"
    :okarina "okarina"
    :stahl-res-m1 "stahlskulptur/resonanz-mallet1"
    :stahl-res-m2 "stahlskulptur/resonanz-mallet2"
    :stahl-res-02 "stahlskulptur/resonanz02"
    :stahl-res-spitz "stahlskulptur/resonanz-spitz"
    :stahl-res-stick "stahlskulptur/resonanz-stick"
    :stahl-trocken "stahlskulptur/trocken"
    :stahl-schrapp "stahlskulptur/schrapp"
    :teppich-git-perk "teppich-gitarre/perkussiv"
    :teppich-git-ton "teppich-gitarre/ton"
    :teppich-git-bogen-dist "teppich-gitarre/bogen-dist"
    :teppich-git-bogen-rausch "teppich-gitarre/bogen-rausch"
    :wohnzimmer-becken "wohnzimmer/becken"
    :wohnzimmer-felltrommel "wohnzimmer/felltrommel"
    :wohnzimmer-holzstick "wohnzimmer/holzstick"
    :wohnzimmer-holzstick-spitz "wohnzimmer/holzstick-spitz"
    :wohnzimmer-stuhlgit "wohnzimmer/stuhlgitarre"
    :wohnzimmer-stuhlharfe "wohnzimmer/stuhlharfe"
    :wohnzimmer-stuhlharfe-bogen "wohnzimmer/stuhlharfe-bogen"
    :zink-yi "perkussion/generell"))

(progn
  (load-poolplayer-sounds "../../snd/bo-samples" *sound-type-dirs*)

  (defparameter *pool0* (collect-pool :wohnzimmer-holzstick))
  (defparameter *pool1* (collect-pool :aitken-gsp))
  (defparameter *pool2* (collect-pool :aitken-mallet))
  (defparameter *pool3* (collect-pool :stahl-trocken))
  (defparameter *pool4* (collect-pool :atoui-feder))
  (defparameter *pool5* (collect-pool :stahl-res-m1))
  (defparameter *pool6* (collect-pool :stahl-res-m2))
  (defparameter *pool7* (collect-pool :stahl-res-02))
  (defparameter *pool8* (collect-pool :stahl-res-spitz))
  (defparameter *pool9* (collect-pool :stahl-res-stick))
  (defparameter *pool10* (collect-pool :stahl-schrapp))
  (defparameter *pool11* (collect-pool :stahl-res-02))
  (defparameter *pool12* (collect-pool :teppich-git-perk))
  (defparameter *pool13* (collect-pool :teppich-git-ton))
  (defparameter *pool14* (collect-pool :wohnzimmer-becken))
  (defparameter *pool15* (collect-pool :wohnzimmer-felltrommel))
  (defparameter *pool16* (collect-pool :wohnzimmer-holzstick))
  (defparameter *pool17* (collect-pool :wohnzimmer-stuhlgit))
  (defparameter *pool18* (collect-pool :wohnzimmer-stuhlharfe))
  (defparameter *pool19* (collect-pool :wohnzimmer-holzstick-spitz))
  (defparameter *pool20* (collect-pool :wohnzimmer-stuhlharfe-bogen))
  (defparameter *pool21* (collect-pool :teppich-git-bogen-dist))
  (defparameter *pool22* (collect-pool :teppich-git-bogen-rausch))
  (defparameter *pool30* (collect-pool
                          :wohnzimmer-holzstick
                          :wohnzimmer-holzstick-spitz
                          :aitken-mallet
                          :stahl-trocken
                          :atoui-feder
                          :stahl-res-m1
                          :stahl-res-m2
                          :stahl-res-02
                          :stahl-res-stick
                          :stahl-res-spitz
                          :wohnzimmer-stuhlharfe
                          :teppich-git-ton
                          :teppich-git-perk))
  (defparameter *pool31* (collect-pool :zink-yi)))

(defparameter *pool0-idxs* (mapcar #'buf-idx (coerce *pool0* 'list)))
(defparameter *pool1-idxs* (mapcar #'buf-idx (coerce *pool1* 'list)))
(defparameter *pool2-idxs* (mapcar #'buf-idx (coerce *pool2* 'list)))
(defparameter *pool31-idxs* (mapcar #'buf-idx (coerce *pool31* 'list)))

(cd "~/quicklisp/local-projects/clamps/packages/cl-poolplayer/")
(init-poolplayer)

(play-buffer-stretch-env-pan-out* (elt *pool0* 0) :amp -6 :pan 0.5)
(defparameter holz-ex-01
  (make-song
   :name "holz-ex-01"
   :durfn (lambda () (r-exp 9 12))
   :afterfn (lambda () (r-exp 3 5))
   :beforefn (lambda () (r-exp 3 5))
   :playfn
   (lambda (dur &key (player-type 'eventplayer))
     (let* ((params (list
                     :g1 *pool0*
                     :outfn #'stereo-out
                     :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6)))))
            (repeat (+ 9 (random 4)))
            (players (loop for i below repeat collect (make-instance player-type))))
       (labels ((recurse-fn (time idx p)
                  (if (< idx repeat)
                      (let* ((duration (n-exp (/ idx (1- repeat)) (/ dur 5) dur))
                             (next (+ time (* 0.2 duration))))
                        (apply #'preset-play (first p) 0 duration
                               :transpfn (lambda (x) (n-lin x (- -13 (* 1.7 idx)) 30))
                               :amp (+ (random 6) -6 (* idx -1.5))
                               :time time
                               params)
                        (if (eql player-type 'eventplotter)
                            (recurse-fn next (incf idx) (cdr p))
                            (at next
                                #'recurse-fn next (incf idx) (cdr p)))))))
         (recurse-fn (now) 0 players))))))

#|
(defun plot-song (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort (mapcar *events* (lambda (x) (cons (- (first x) time) '(halle)))) #'< :key #'first)))

(defun plot-song (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort (mapcar #'(lambda (x) (append (list :time (float (- (first x) time) 1.0)
                                         :buffer-idx (buffer-idx (getf (cdr x) :buffer)))
                                 (progn (remf (cdr x) :buffer)
                                        (cdr x)))) *events*) #'< :key (lambda (x) (getf x :time)))))
(defun cm-collect (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort (mapcar #'(lambda (x) (apply #'make-instance 'cm:poolevt
                                  :time (float (- (first x) time) 1.0)
                                  :buffer-idx (buffer-idx (getf (cdr x) :buffer))
                                  (progn (remf (cdr x) :buffer)
                                         (cdr x)))) *events*) #'< :key cm::time)))



(in-package :cl-poolplayer)

(defun cm-collect (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort
     (mapcar #'(lambda (x) (apply #'make-instance 'cm:poolevt
                             :time (float (- (first x) time) 1.0)
                             :buffer-idx (buffer-idx (getf (cdr x) :buffer))
                             (progn (remf (cdr x) :buffer)
                                    (cdr x))))
             *events*)
     #'< :key (lambda (x) (sv x cm::time)))))
|#

(play-song holz-ex-01)
(perform)
(make-instance 'cm:poolevt)


(cl-poolplayer::distributed-play (:buffer #<incudine:buffer :frames 1728 :channels 1 :sr 48000.0> :amp -11.5 :transp -13.137001 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33991754 :out1 0 :out2 1))


(sprout (cm-collect holz-ex-01))

(sprout
 holz-ex-01)
(events
 (cm-collect holz-ex-01)
 *rts-out*)

(write-event)

(events
 (cm-collect holz-ex-01)
 "/tmp/test.svg"
 :piano-roll-vis nil
 :staff-system-vis nil
 :bar-lines-vis nil
 :showgrid nil)

(cm::close-io)

(apply #'make-instance 'cm:poolevt :time 0.0 :buffer-idx 363 '(:amp -9.0 :transp -13.0 :start 0 :end 0 :stretch
        1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.75942147 :out1 0 :out2
        1))

((:time 0.0 :buffer-idx 363 :amp -9.0 :transp -13.0 :start 0 :end 0 :stretch
        1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.75942147 :out1 0 :out2
        1)
 (:time 0.03 :buffer-idx 362 :amp -6.0 :transp -12.300736 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.80287063 :out1
        0 :out2 1)
 (:time 0.06137009 :buffer-idx 362 :amp -5.0 :transp -11.569538 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14312434 :out1
        0 :out2 1)
 (:time 0.0948884 :buffer-idx 363 :amp -11.0 :transp -10.788267 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16851985 :out1
        0 :out2 1)
 (:time 0.1325799 :buffer-idx 361 :amp -8.0 :transp -9.909723 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7505233 :out1 0
        :out2 1)
 (:time 0.17606086 :buffer-idx 363 :amp -11.0 :transp -8.8962345 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8900819 :out1
        0 :out2 1)
 (:time 0.22479069 :buffer-idx 362 :amp -5.0 :transp -7.7604017 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.55537975 :out1
        0 :out2 1)
 (:time 0.27757734 :buffer-idx 363 :amp -4.0 :transp -6.530009 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5659653 :out1 0
        :out2 1)
 (:time 0.32724702 :buffer-idx 363 :amp -10.0 :transp -5.372269 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77032995 :out1
        0 :out2 1)
 (:time 0.36895958 :buffer-idx 363 :amp -14.5 :transp -14.7 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47612727 :out1
        0 :out2 1)
 (:time 0.39711547 :buffer-idx 362 :amp -5.0 :transp -3.74372 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9974843 :out1 0
        :out2 1)
 (:time 0.39895958 :buffer-idx 363 :amp -13.5 :transp -14.072032 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15854001
        :out1 0 :out2 1)
 (:time 0.43030182 :buffer-idx 362 :amp -10.5 :transp -13.415969 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12359214
        :out1 0 :out2 1)
 (:time 0.46414083 :buffer-idx 361 :amp -7.5 :transp -12.707642 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.004950881 :out1
        0 :out2 1)
 (:time 0.4645645 :buffer-idx 362 :amp -12.0 :transp -2.171564 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.48007572 :out1
        0 :out2 1)
 (:time 0.502122 :buffer-idx 362 :amp -7.5 :transp -11.912611 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7069503 :out1 0
        :out2 1)
 (:time 0.5389354 :buffer-idx 363 :amp -4.5 :transp -11.142023 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33707404 :out1
        0 :out2 1)
 (:time 0.577385 :buffer-idx 362 :amp -6.0 :transp 0.45814514 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37323093 :out1
        0 :out2 1)
 (:time 0.5834312 :buffer-idx 362 :amp -5.5 :transp -10.210625 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6591619 :out1 0
        :out2 1)
 (:time 0.6270466 :buffer-idx 361 :amp -14.5 :transp -9.297657 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45719612 :out1
        0 :out2 1)
 (:time 0.67173916 :buffer-idx 362 :amp -4.5 :transp -8.36214 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8429146 :out1 0
        :out2 1)
 (:time 0.69655323 :buffer-idx 361 :amp -3.0 :transp 3.2358112 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15955722 :out1
        0 :out2 1)
 (:time 0.716674 :buffer-idx 361 :amp -8.5 :transp -7.4215527 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4862802 :out1 0
        :out2 1)
 (:time 0.76045483 :buffer-idx 363 :amp -9.5 :transp -6.505121 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33870268 :out1
        0 :out2 1)
 (:time 0.78439695 :buffer-idx 363 :amp -11.0 :transp 5.2833424 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.86020184 :out1
        0 :out2 1)
 (:time 0.7960516 :buffer-idx 363 :amp -9.0 :transp -16.4 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24534035 :out1
        0 :out2 1)
 (:time 0.8195907 :buffer-idx 362 :amp -10.5 :transp -5.267275 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51635695 :out1
        0 :out2 1)
 (:time 0.82605165 :buffer-idx 361 :amp -8.0 :transp -15.836874 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.41776276 :out1
        0 :out2 1)
 (:time 0.8571145 :buffer-idx 363 :amp -14.0 :transp -15.2537985 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33799005
        :out1 0 :out2 1)
 (:time 0.8822045 :buffer-idx 363 :amp -4.5 :transp -3.956627 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.58384323 :out1
        0 :out2 1)
 (:time 0.8889646 :buffer-idx 361 :amp -5.0 :transp -14.655945 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9157704 :out1 0
        :out2 1)
 (:time 0.90575814 :buffer-idx 361 :amp -4.0 :transp 8.112125 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.084195375 :out1
        0 :out2 1)
 (:time 0.92300063 :buffer-idx 363 :amp -15.0 :transp -14.017061 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.64946675
        :out1 0 :out2 1)
 (:time 0.9557038 :buffer-idx 363 :amp -13.5 :transp -2.4181204 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3021034 :out1 0
        :out2 1)
 (:time 0.9583752 :buffer-idx 361 :amp -16.0 :transp -13.353049 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38925898 :out1
        0 :out2 1)
 (:time 0.9938553 :buffer-idx 363 :amp -9.0 :transp -12.687059 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59943175 :out1
        0 :out2 1)
 (:time 1.0185649 :buffer-idx 361 :amp -14.5 :transp -1.1022949 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.051269174 :out1
        0 :out2 1)
 (:time 1.0325862 :buffer-idx 361 :amp -15.0 :transp -11.960048 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.90051365 :out1
        0 :out2 1)
 (:time 1.079429 :buffer-idx 361 :amp -8.0 :transp -11.080769 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8764496 :out1 0
        :out2 1)
 (:time 1.134134 :buffer-idx 361 :amp -13.0 :transp -10.053909 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8856679 :out1 0
        :out2 1)
 (:time 1.1381041 :buffer-idx 361 :amp -4.0 :transp 13.527826 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46403062 :out1
        0 :out2 1)
 (:time 1.1546526 :buffer-idx 363 :amp -3.5 :transp 1.7463274 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2134074 :out1 0
        :out2 1)
 (:time 1.1882386 :buffer-idx 361 :amp -8.0 :transp -9.038319 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7400391 :out1 0
        :out2 1)
 (:time 1.2445941 :buffer-idx 363 :amp -10.0 :transp -7.9804792 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29924 :out1 0
        :out2 1)
 (:time 1.2904354 :buffer-idx 362 :amp -14.5 :transp -18.1 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.03015995 :out1
        0 :out2 1)
 (:time 1.3031785 :buffer-idx 361 :amp -15.0 :transp -6.880802 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22702885 :out1
        0 :out2 1)
 (:time 1.3204354 :buffer-idx 363 :amp -15.5 :transp -17.5957 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.969188 :out1 0
        :out2 1)
 (:time 1.3484586 :buffer-idx 361 :amp -6.5 :transp 5.8031244 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.61782277 :out1
        0 :out2 1)
 (:time 1.3513129 :buffer-idx 361 :amp -12.5 :transp -17.07665 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8745003 :out1 0
        :out2 1)
 (:time 1.357054 :buffer-idx 363 :amp -8.0 :transp -5.8695135 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.94654906 :out1
        0 :out2 1)
 (:time 1.3828598 :buffer-idx 363 :amp -19.5 :transp -16.546345 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.76188016 :out1
        0 :out2 1)
 (:time 1.4155773 :buffer-idx 363 :amp -13.5 :transp -15.996365 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3960215 :out1 0
        :out2 1)
 (:time 1.4387016 :buffer-idx 361 :amp -6.0 :transp -4.336919 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15234733 :out1
        0 :out2 1)
 (:time 1.4523194 :buffer-idx 361 :amp -12.5 :transp -15.378728 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3731376 :out1 0
        :out2 1)
 (:time 1.486946 :buffer-idx 363 :amp -15.5 :transp -14.796656 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.96834433 :out1
        0 :out2 1)
 (:time 1.4968281 :buffer-idx 362 :amp -6.0 :transp -3.2458363 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.97197545 :out1
        0 :out2 1)
 (:time 1.506492 :buffer-idx 361 :amp -3.5 :transp 9.111117 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08846164 :out1
        0 :out2 1)
 (:time 1.5234246 :buffer-idx 361 :amp -19.5 :transp -14.183449 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5173172 :out1 0
        :out2 1)
 (:time 1.52451 :buffer-idx 363 :amp -13.0 :transp 22.534477 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.113291144 :out1
        0 :out2 1)
 (:time 1.5608547 :buffer-idx 361 :amp -11.5 :transp -13.554249 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26523829 :out1
        0 :out2 1)
 (:time 1.5618905 :buffer-idx 361 :amp -9.0 :transp -2.024559 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9248651 :out1 0
        :out2 1)
 (:time 1.608346 :buffer-idx 363 :amp -11.5 :transp -12.75592 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.048826694 :out1
        0 :out2 1)
 (:time 1.635891 :buffer-idx 363 :amp -8.0 :transp -0.6355095 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4894172 :out1 0
        :out2 1)
 (:time 1.6506238 :buffer-idx 362 :amp -21.5 :transp -12.045229 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3269918 :out1 0
        :out2 1)
 (:time 1.7044865 :buffer-idx 363 :amp -11.5 :transp 13.2555895 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.067056656 :out1
        0 :out2 1)
 (:time 1.7087755 :buffer-idx 361 :amp -14.5 :transp -11.067698 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8177804 :out1 0
        :out2 1)
 (:time 1.737192 :buffer-idx 362 :amp -16.0 :transp 1.2659969 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.69752026 :out1
        0 :out2 1)
 (:time 1.7519001 :buffer-idx 363 :amp -17.5 :transp -10.342773 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.017427564 :out1
        0 :out2 1)
 (:time 1.8000697 :buffer-idx 361 :amp -16.5 :transp -9.533042 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47013366 :out1
        0 :out2 1)
 (:time 1.8157793 :buffer-idx 363 :amp -8.0 :transp 29.323616 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38885474 :out1
        0 :out2 1)
 (:time 1.8467004 :buffer-idx 362 :amp -11.5 :transp -8.749177 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21237826 :out1
        0 :out2 1)
 (:time 1.8627133 :buffer-idx 363 :amp -18.0 :transp -19.8 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8731853 :out1 0
        :out2 1)
 (:time 1.8906188 :buffer-idx 363 :amp -5.0 :transp 4.1459465 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.92050517 :out1
        0 :out2 1)
 (:time 1.8927133 :buffer-idx 363 :amp -8.0 :transp -19.348944 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9560714 :out1 0
        :out2 1)
 (:time 1.9184859 :buffer-idx 361 :amp -22.5 :transp -7.5424623 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8253561 :out1 0
        :out2 1)
 (:time 1.9240892 :buffer-idx 361 :amp -19.0 :transp -18.877201 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8249488 :out1 0
        :out2 1)
 (:time 1.9482893 :buffer-idx 363 :amp -3.5 :transp 18.358932 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07078719 :out1
        0 :out2 1)
 (:time 1.9565909 :buffer-idx 361 :amp -10.0 :transp -18.38853 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.75902295 :out1
        0 :out2 1)
 (:time 1.9827155 :buffer-idx 362 :amp -13.5 :transp -6.462763 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5002936 :out1 0
        :out2 1)
 (:time 1.9902549 :buffer-idx 362 :amp -12.0 :transp -17.882385 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38466346 :out1
        0 :out2 1)
 (:time 2.0244217 :buffer-idx 363 :amp -18.0 :transp -17.36868 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33531165 :out1
        0 :out2 1)
 (:time 2.0319657 :buffer-idx 363 :amp -10.0 :transp 6.7991505 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.30293465 :out1
        0 :out2 1)
 (:time 2.062926 :buffer-idx 362 :amp -18.0 :transp -16.789757 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.737769 :out1 0
        :out2 1)
 (:time 2.0722644 :buffer-idx 362 :amp -15.5 :transp -4.957445 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08565831 :out1
        0 :out2 1)
 (:time 2.103028 :buffer-idx 362 :amp -8.0 :transp -16.186817 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.68492115 :out1
        0 :out2 1)
 (:time 2.1357024 :buffer-idx 361 :amp -16.0 :transp 8.74637 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.98548007 :out1
        0 :out2 1)
 (:time 2.1443744 :buffer-idx 361 :amp -14.0 :transp -15.565165 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8771062 :out1 0
        :out2 1)
 (:time 2.1717753 :buffer-idx 362 :amp -14.5 :transp -3.2846632 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7155992 :out1 0
        :out2 1)
 (:time 2.184484 :buffer-idx 363 :amp -15.0 :transp -14.962109 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12033105 :out1
        0 :out2 1)
 (:time 2.2342935 :buffer-idx 363 :amp -11.0 :transp -14.213211 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10063934 :out1
        0 :out2 1)
 (:time 2.2448924 :buffer-idx 363 :amp -15.5 :transp -2.0555649 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33299506 :out1
        0 :out2 1)
 (:time 2.2822018 :buffer-idx 362 :amp -8.0 :transp -13.492899 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5454718 :out1 0
        :out2 1)
 (:time 2.3255465 :buffer-idx 363 :amp -19.5 :transp -0.69977 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7664727 :out1 0
        :out2 1)
 (:time 2.329347 :buffer-idx 361 :amp -19.0 :transp -12.784065 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33678293 :out1
        0 :out2 1)
 (:time 2.379712 :buffer-idx 361 :amp -9.0 :transp -12.026812 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.965181 :out1 0
        :out2 1)
 (:time 2.3805573 :buffer-idx 363 :amp -7.0 :transp 13.3425045 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.684106 :out1 0
        :out2 1)
 (:time 2.4090285 :buffer-idx 361 :amp -21.5 :transp 0.7035637 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.016829014 :out1
        0 :out2 1)
 (:time 2.4289546 :buffer-idx 363 :amp -17.0 :transp -11.286441 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23997629 :out1
        0 :out2 1)
 (:time 2.4825368 :buffer-idx 363 :amp -16.0 :transp -10.48082 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.81133366 :out1
        0 :out2 1)
 (:time 2.4931014 :buffer-idx 363 :amp -18.5 :transp 2.1168327 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5498662 :out1 0
        :out2 1)
 (:time 2.5251582 :buffer-idx 362 :amp -11.5 :transp -21.5 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.60277605 :out1
        0 :out2 1)
 (:time 2.531412 :buffer-idx 363 :amp -8.0 :transp -9.745975 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.802814 :out1 0
        :out2 1)
 (:time 2.5551581 :buffer-idx 362 :amp -18.5 :transp -21.097036 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.72662556 :out1
        0 :out2 1)
 (:time 2.581708 :buffer-idx 361 :amp -19.5 :transp 3.6063118 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.78475523 :out1
        0 :out2 1)
 (:time 2.586426 :buffer-idx 362 :amp -20.5 :transp -20.677042 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7013602 :out1 0
        :out2 1)
 (:time 2.6062706 :buffer-idx 362 :amp -13.0 :transp -8.620456 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.64109635 :out1
        0 :out2 1)
 (:time 2.6177413 :buffer-idx 361 :amp -13.5 :transp -20.25641 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5740073 :out1 0
        :out2 1)
 (:time 2.6503866 :buffer-idx 361 :amp -16.5 :transp -19.817917 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.78737545 :out1
        0 :out2 1)
 (:time 2.6699538 :buffer-idx 362 :amp -14.0 :transp -7.662966 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17477381 :out1
        0 :out2 1)
 (:time 2.6852005 :buffer-idx 363 :amp -14.5 :transp -19.35029 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.74811804 :out1
        0 :out2 1)
 (:time 2.7199268 :buffer-idx 362 :amp -13.5 :transp -18.883842 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9161242 :out1 0
        :out2 1)
 (:time 2.756557 :buffer-idx 363 :amp -21.5 :transp -18.39182 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.70150006 :out1
        0 :out2 1)
 (:time 2.768947 :buffer-idx 363 :amp -12.0 :transp -6.1745834 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.27024794 :out1
        0 :out2 1)
 (:time 2.7937355 :buffer-idx 363 :amp -21.5 :transp 7.170496 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6308713 :out1 0
        :out2 1)
 (:time 2.797564 :buffer-idx 363 :amp -21.5 :transp -17.841007 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13049328 :out1
        0 :out2 1)
 (:time 2.8403747 :buffer-idx 362 :amp -16.5 :transp -17.265972 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.013661623 :out1
        0 :out2 1)
 (:time 2.8759832 :buffer-idx 361 :amp -17.0 :transp -4.5652695 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.48034906 :out1
        0 :out2 1)
 (:time 2.8783221 :buffer-idx 361 :amp -20.5 :transp -16.756256 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08616042 :out1
        0 :out2 1)
 (:time 2.9236748 :buffer-idx 362 :amp -17.5 :transp -16.147074 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07693517 :out1
        0 :out2 1)
 (:time 2.9393368 :buffer-idx 362 :amp -8.0 :transp -3.6127357 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20785785 :out1
        0 :out2 1)
 (:time 2.962985 :buffer-idx 363 :amp -16.5 :transp -15.619054 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.85205925 :out1
        0 :out2 1)
 (:time 3.0133116 :buffer-idx 363 :amp -16.5 :transp -14.943059 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8673054 :out1 0
        :out2 1)
 (:time 3.0260987 :buffer-idx 362 :amp -20.5 :transp 11.076527 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.05306995 :out1
        0 :out2 1)
 (:time 3.055335 :buffer-idx 361 :amp -21.5 :transp -14.378597 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9891273 :out1 0
        :out2 1)
 (:time 3.0673594 :buffer-idx 361 :amp -18.0 :transp -1.6878872 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.915133 :out1 0
        :out2 1)
 (:time 3.1113982 :buffer-idx 361 :amp -19.5 :transp -13.625549 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.70207965 :out1
        0 :out2 1)
 (:time 3.1788015 :buffer-idx 362 :amp -14.5 :transp -12.72018 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47890747 :out1
        0 :out2 1)
 (:time 3.2094197 :buffer-idx 362 :amp -10.0 :transp 0.44801903 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.91524625 :out1
        0 :out2 1)
 (:time 3.2232647 :buffer-idx 363 :amp -16.5 :transp -12.122944 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31086648 :out1
        0 :out2 1)
 (:time 3.2619946 :buffer-idx 363 :amp -8.0 :transp 29.887823 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6718402 :out1 0
        :out2 1)
 (:time 3.281829 :buffer-idx 361 :amp -17.5 :transp -11.336298 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0671283 :out1 0
        :out2 1)
 (:time 3.2919765 :buffer-idx 361 :amp -23.0 :transp -23.2 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22701609 :out1
        0 :out2 1)
 (:time 3.3056626 :buffer-idx 362 :amp -18.5 :transp 15.775999 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.012794852 :out1
        0 :out2 1)
 (:time 3.3219764 :buffer-idx 362 :amp -16.0 :transp -22.840395 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7946634 :out1 0
        :out2 1)
 (:time 3.3431537 :buffer-idx 362 :amp -10.5 :transp -10.51258 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7800542 :out1 0
        :out2 1)
 (:time 3.3517456 :buffer-idx 361 :amp -8.0 :transp 2.5879211 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14368856 :out1
        0 :out2 1)
 (:time 3.3527987 :buffer-idx 362 :amp -16.0 :transp -22.47093 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6538911 :out1 0
        :out2 1)
 (:time 3.3842187 :buffer-idx 361 :amp -15.0 :transp -22.094303 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.984431 :out1 0
        :out2 1)
 (:time 3.394119 :buffer-idx 362 :amp -15.5 :transp -9.828006 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.025875926 :out1
        0 :out2 1)
 (:time 3.4157083 :buffer-idx 361 :amp -23.0 :transp -21.716839 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8226725 :out1 0
        :out2 1)
 (:time 3.4491932 :buffer-idx 363 :amp -19.0 :transp -21.31546 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.25151396 :out1
        0 :out2 1)
 (:time 3.4646652 :buffer-idx 362 :amp -13.5 :transp 18.448832 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44051778 :out1
        0 :out2 1)
 (:time 3.4818838 :buffer-idx 362 :amp -21.5 :transp -8.64914 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.075246096 :out1
        0 :out2 1)
 (:time 3.4823797 :buffer-idx 363 :amp -23.0 :transp -20.917656 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26723576 :out1
        0 :out2 1)
 (:time 3.5173266 :buffer-idx 361 :amp -19.0 :transp -20.498753 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.35145926 :out1
        0 :out2 1)
 (:time 3.5211134 :buffer-idx 361 :amp -16.0 :transp 5.1343994 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8404844 :out1 0
        :out2 1)
 (:time 3.5553117 :buffer-idx 363 :amp -20.0 :transp -20.043428 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9874891 :out1 0
        :out2 1)
 (:time 3.5719318 :buffer-idx 363 :amp -18.5 :transp -7.439601 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08256495 :out1
        0 :out2 1)
 (:time 3.591926 :buffer-idx 363 :amp -24.0 :transp -19.604538 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.98553073 :out1
        0 :out2 1)
 (:time 3.6285167 :buffer-idx 362 :amp -22.0 :transp -19.16593 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26437318 :out1
        0 :out2 1)
 (:time 3.6481028 :buffer-idx 362 :amp -16.5 :transp -6.416462 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9109901 :out1 0
        :out2 1)
 (:time 3.6675441 :buffer-idx 362 :amp -13.0 :transp 7.3360195 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.79264975 :out1
        0 :out2 1)
 (:time 3.672381 :buffer-idx 362 :amp -15.0 :transp -18.640133 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.90701747 :out1
        0 :out2 1)
 (:time 3.712225 :buffer-idx 363 :amp -17.0 :transp -18.162527 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7966517 :out1 0
        :out2 1)
 (:time 3.7612596 :buffer-idx 363 :amp -21.0 :transp -17.574755 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8495754 :out1 0
        :out2 1)
 (:time 3.7764578 :buffer-idx 361 :amp -12.5 :transp -4.6923847 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21239829 :out1
        0 :out2 1)
 (:time 3.8015316 :buffer-idx 363 :amp -18.0 :transp 9.350552 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2460618 :out1 0
        :out2 1)
 (:time 3.8111522 :buffer-idx 362 :amp -14.0 :transp -16.976696 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36625433 :out1
        0 :out2 1)
 (:time 3.8636594 :buffer-idx 362 :amp -23.0 :transp -16.3473 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24411857 :out1
        0 :out2 1)
 (:time 3.9065282 :buffer-idx 361 :amp -24.0 :transp -15.833437 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.008383274 :out1
        0 :out2 1)
 (:time 3.9335463 :buffer-idx 362 :amp -13.5 :transp -2.5823498 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8811765 :out1 0
        :out2 1)
 (:time 3.9625456 :buffer-idx 363 :amp -16.0 :transp -15.161961 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.19279039 :out1
        0 :out2 1)
 (:time 4.0285554 :buffer-idx 361 :amp -20.0 :transp -14.370709 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12181342 :out1
        0 :out2 1)
 (:time 4.0403438 :buffer-idx 363 :amp -11.5 :transp -1.1478329 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.50122714 :out1
        0 :out2 1)
 (:time 4.1007943 :buffer-idx 363 :amp -19.0 :transp -13.504791 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.717944 :out1 0
        :out2 1)
 (:time 4.152132 :buffer-idx 361 :amp -18.5 :transp 0.3537197 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.130342 :out1 0
        :out2 1)
 (:time 4.1698017 :buffer-idx 362 :amp -21.0 :transp -12.677606 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1263076 :out1 0
        :out2 1)
 (:time 4.1796126 :buffer-idx 363 :amp -21.5 :transp -24.900002 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.017286658 :out1
        0 :out2 1)
 (:time 4.209613 :buffer-idx 361 :amp -16.5 :transp -24.579414 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.11681402 :out1
        0 :out2 1)
 (:time 4.2178392 :buffer-idx 362 :amp -17.0 :transp -12.101784 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.74293745 :out1
        0 :out2 1)
 (:time 4.240098 :buffer-idx 362 :amp -16.5 :transp -24.253645 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4251014 :out1 0
        :out2 1)
 (:time 4.2464147 :buffer-idx 362 :amp -11.5 :transp 1.6201382 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.837886 :out1 0
        :out2 1)
 (:time 4.271211 :buffer-idx 361 :amp -20.5 :transp -23.921164 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10844636 :out1
        0 :out2 1)
 (:time 4.302957 :buffer-idx 361 :amp -24.5 :transp -23.581915 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.89352334 :out1
        0 :out2 1)
 (:time 4.304893 :buffer-idx 361 :amp -17.0 :transp -11.058282 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2083664 :out1 0
        :out2 1)
 (:time 4.3346705 :buffer-idx 362 :amp -22.5 :transp -23.243021 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6855439 :out1 0
        :out2 1)
 (:time 4.3634014 :buffer-idx 363 :amp -18.0 :transp -10.35695 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.569538 :out1 0
        :out2 1)
 (:time 4.3687973 :buffer-idx 362 :amp -26.5 :transp -22.878332 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9592397 :out1 0
        :out2 1)
 (:time 4.4052486 :buffer-idx 362 :amp -17.5 :transp -22.488808 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0112217665
        :out1 0 :out2 1)
 (:time 4.440993 :buffer-idx 361 :amp -22.5 :transp -22.106836 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5437461 :out1 0
        :out2 1)
 (:time 4.4412456 :buffer-idx 363 :amp -15.0 :transp -9.4238405 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9748932 :out1 0
        :out2 1)
 (:time 4.4610395 :buffer-idx 362 :amp -16.0 :transp 19.266396 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.52514863 :out1
        0 :out2 1)
 (:time 4.476208 :buffer-idx 362 :amp -21.5 :transp -21.730516 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18001091 :out1
        0 :out2 1)
 (:time 4.511342 :buffer-idx 363 :amp -16.5 :transp -21.355068 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.510993 :out1 0
        :out2 1)
 (:time 4.5141425 :buffer-idx 362 :amp -21.5 :transp 5.2162914 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5165014 :out1 0
        :out2 1)
 (:time 4.528192 :buffer-idx 361 :amp -23.0 :transp -8.381619 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9312421 :out1 0
        :out2 1)
 (:time 4.546864 :buffer-idx 363 :amp -22.5 :transp -20.975473 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43956637 :out1
        0 :out2 1)
 (:time 4.583255 :buffer-idx 362 :amp -23.5 :transp -20.58659 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9200047 :out1 0
        :out2 1)
 (:time 4.623876 :buffer-idx 363 :amp -19.5 :transp -20.152502 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9534992 :out1 0
        :out2 1)
 (:time 4.6341214 :buffer-idx 362 :amp -25.0 :transp -7.1118584 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13426542 :out1
        0 :out2 1)
 (:time 4.669676 :buffer-idx 361 :amp -17.5 :transp -19.663074 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26976764 :out1
        0 :out2 1)
 (:time 4.7007957 :buffer-idx 361 :amp -17.0 :transp -6.312639 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77842355 :out1
        0 :out2 1)
 (:time 4.712012 :buffer-idx 361 :amp -21.5 :transp -19.210663 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9716033 :out1 0
        :out2 1)
 (:time 4.7525034 :buffer-idx 362 :amp -21.5 :transp -18.777962 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21275198 :out1
        0 :out2 1)
 (:time 4.803698 :buffer-idx 362 :amp -20.5 :transp -18.230886 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45606184 :out1
        0 :out2 1)
 (:time 4.8197064 :buffer-idx 363 :amp -15.0 :transp -4.88727 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.79415727 :out1
        0 :out2 1)
 (:time 4.8288918 :buffer-idx 362 :amp -14.5 :transp 9.444044 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51477253 :out1
        0 :out2 1)
 (:time 4.8463235 :buffer-idx 362 :amp -20.5 :transp -17.775377 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.85235417 :out1
        0 :out2 1)
 (:time 4.89092 :buffer-idx 361 :amp -26.5 :transp -17.29881 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49562263 :out1
        0 :out2 1)
 (:time 4.92549 :buffer-idx 361 :amp -18.0 :transp -3.6192532 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37878072 :out1
        0 :out2 1)
 (:time 4.93448 :buffer-idx 361 :amp -23.5 :transp -16.833313 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7885884 :out1 0
        :out2 1)
 (:time 4.9853735 :buffer-idx 361 :amp -21.5 :transp -16.289455 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.66953206 :out1
        0 :out2 1)
 (:time 5.027616 :buffer-idx 362 :amp -19.0 :transp -2.3950825 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2459135 :out1 0
        :out2 1)
 (:time 5.0412006 :buffer-idx 363 :amp -21.5 :transp -15.692876 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3568741 :out1 0
        :out2 1)
 (:time 5.085915 :buffer-idx 362 :amp -23.5 :transp -15.215047 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7067194 :out1 0
        :out2 1)
 (:time 5.1408696 :buffer-idx 363 :amp -19.5 :transp -14.627791 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.674538 :out1 0
        :out2 1)
 (:time 5.201448 :buffer-idx 363 :amp -27.5 :transp -13.980435 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5404992 :out1 0
        :out2 1)
 (:time 5.2071033 :buffer-idx 363 :amp -21.0 :transp -26.6 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6159117 :out1 0
        :out2 1)
 (:time 5.217402 :buffer-idx 363 :amp -23.0 :transp -0.12013435 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.003407836 :out1
        0 :out2 1)
 (:time 5.2371035 :buffer-idx 363 :amp -20.0 :transp -26.314474 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21699047 :out1
        0 :out2 1)
 (:time 5.267909 :buffer-idx 363 :amp -21.0 :transp -26.021278 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6209258 :out1 0
        :out2 1)
 (:time 5.2766023 :buffer-idx 362 :amp -26.5 :transp -13.177321 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.71125245 :out1
        0 :out2 1)
 (:time 5.299265 :buffer-idx 363 :amp -20.0 :transp -25.722847 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23256123 :out1
        0 :out2 1)
 (:time 5.330469 :buffer-idx 361 :amp -19.0 :transp -25.425858 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16117275 :out1
        0 :out2 1)
 (:time 5.346544 :buffer-idx 362 :amp -15.5 :transp 16.397213 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.05610311 :out1
        0 :out2 1)
 (:time 5.3580074 :buffer-idx 362 :amp -20.5 :transp -12.307405 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2666155 :out1 0
        :out2 1)
 (:time 5.362349 :buffer-idx 361 :amp -25.0 :transp -25.12244 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8729497 :out1 0
        :out2 1)
 (:time 5.3925314 :buffer-idx 363 :amp -16.0 :transp 1.9791222 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.27042687 :out1
        0 :out2 1)
 (:time 5.394273 :buffer-idx 362 :amp -21.0 :transp -24.818605 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44615364 :out1
        0 :out2 1)
 (:time 5.4145927 :buffer-idx 362 :amp -16.5 :transp -11.702725 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2558155 :out1 0
        :out2 1)
 (:time 5.4285197 :buffer-idx 362 :amp -19.0 :transp -24.492657 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6859485 :out1 0
        :out2 1)
 (:time 5.4632425 :buffer-idx 361 :amp -24.0 :transp -24.162182 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4231726 :out1 0
        :out2 1)
 (:time 5.498554 :buffer-idx 362 :amp -20.0 :transp -23.826097 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.60557234 :out1
        0 :out2 1)
 (:time 5.5127597 :buffer-idx 363 :amp -22.5 :transp -10.65369 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3190869 :out1 0
        :out2 1)
 (:time 5.532262 :buffer-idx 362 :amp -26.0 :transp -23.505283 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.75333965 :out1
        0 :out2 1)
 (:time 5.5688105 :buffer-idx 362 :amp -19.0 :transp -23.15743 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4600234 :out1 0
        :out2 1)
 (:time 5.6068873 :buffer-idx 361 :amp -23.0 :transp -22.795033 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18451154 :out1
        0 :out2 1)
 (:time 5.6171374 :buffer-idx 363 :amp -25.5 :transp -9.538286 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3500744 :out1 0
        :out2 1)
 (:time 5.6473017 :buffer-idx 361 :amp -23.0 :transp -22.410387 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.34952188 :out1
        0 :out2 1)
 (:time 5.6777062 :buffer-idx 362 :amp -27.5 :transp -8.891033 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.85686815 :out1
        0 :out2 1)
 (:time 5.6829467 :buffer-idx 363 :amp -17.0 :transp 5.4602947 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5758865 :out1 0
        :out2 1)
 (:time 5.6852975 :buffer-idx 362 :amp -28.0 :transp -22.048758 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7495116 :out1 0
        :out2 1)
 (:time 5.730123 :buffer-idx 361 :amp -27.0 :transp -21.622128 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8466017 :out1 0
        :out2 1)
 (:time 5.758345 :buffer-idx 361 :amp -24.5 :transp -8.0293045 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5789423 :out1 0
        :out2 1)
 (:time 5.77086 :buffer-idx 361 :amp -28.0 :transp -21.234407 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.123971224 :out1
        0 :out2 1)
 (:time 5.8160615 :buffer-idx 363 :amp -21.0 :transp -20.804203 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.79807556 :out1
        0 :out2 1)
 (:time 5.8594437 :buffer-idx 363 :amp -27.0 :transp -20.391312 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51368594 :out1
        0 :out2 1)
 (:time 5.862528 :buffer-idx 362 :amp -22.5 :transp -6.915987 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9672277 :out1 0
        :out2 1)
 (:time 5.8923 :buffer-idx 362 :amp -22.0 :transp 7.969795 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18748987 :out1
        0 :out2 1)
 (:time 5.9064636 :buffer-idx 362 :amp -26.0 :transp -19.943794 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7225206 :out1 0
        :out2 1)
 (:time 5.9604707 :buffer-idx 363 :amp -27.0 :transp -19.42978 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.050584078 :out1
        0 :out2 1)
 (:time 6.007973 :buffer-idx 363 :amp -29.0 :transp -18.977673 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14034033 :out1
        0 :out2 1)
 (:time 6.024443 :buffer-idx 361 :amp -25.5 :transp -5.1857224 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7682893 :out1 0
        :out2 1)
 (:time 6.050423 :buffer-idx 363 :amp -19.0 :transp -18.57365 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37733912 :out1
        0 :out2 1)
 (:time 6.064889 :buffer-idx 363 :amp -15.0 :transp 10.038593 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.11429453 :out1
        0 :out2 1)
 (:time 6.111823 :buffer-idx 362 :amp -21.0 :transp -17.989273 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7029991 :out1 0
        :out2 1)
 (:time 6.112551 :buffer-idx 361 :amp -17.5 :transp -4.244177 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.32499373 :out1
        0 :out2 1)
 (:time 6.1546555 :buffer-idx 362 :amp -21.0 :transp -17.581612 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20378125 :out1
        0 :out2 1)
 (:time 6.203593 :buffer-idx 362 :amp -27.0 :transp -17.115849 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10836363 :out1
        0 :out2 1)
 (:time 6.255234 :buffer-idx 363 :amp -30.0 :transp -16.624352 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.41744173 :out1
        0 :out2 1)
 (:time 6.2723165 :buffer-idx 363 :amp -24.5 :transp -2.536892 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.80324376 :out1
        0 :out2 1)
 (:time 6.32397 :buffer-idx 361 :amp -21.0 :transp -15.970152 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.85074794 :out1
        0 :out2 1)
 (:time 6.387149 :buffer-idx 363 :amp -29.0 :transp -15.368843 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.65163493 :out1
        0 :out2 1)
 (:time 6.3964834 :buffer-idx 363 :amp -20.5 :transp -28.3 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5127728 :out1 0
        :out2 1)
 (:time 6.426483 :buffer-idx 361 :amp -21.5 :transp -28.045927 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.69839025 :out1
        0 :out2 1)
 (:time 6.43435 :buffer-idx 362 :amp -25.5 :transp -0.8053627 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4493642 :out1 0
        :out2 1)
 (:time 6.443068 :buffer-idx 363 :amp -19.0 :transp -14.836626 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9969598 :out1 0
        :out2 1)
 (:time 6.4569125 :buffer-idx 363 :amp -23.5 :transp -27.788221 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08155775 :out1
        0 :out2 1)
 (:time 6.4882436 :buffer-idx 363 :amp -18.5 :transp -27.522875 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2171427 :out1 0
        :out2 1)
 (:time 6.5025864 :buffer-idx 362 :amp -25.0 :transp -14.270157 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15053272 :out1
        0 :out2 1)
 (:time 6.51014 :buffer-idx 363 :amp -21.0 :transp 15.375771 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2052077 :out1 0
        :out2 1)
 (:time 6.5198402 :buffer-idx 362 :amp -19.5 :transp -27.255283 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51033795 :out1
        0 :out2 1)
 (:time 6.551162 :buffer-idx 363 :amp -26.5 :transp -26.99002 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46435344 :out1
        0 :out2 1)
 (:time 6.584004 :buffer-idx 362 :amp -21.5 :transp -26.711876 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7015691 :out1 0
        :out2 1)
 (:time 6.587733 :buffer-idx 363 :amp -20.0 :transp -13.459773 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13229048 :out1
        0 :out2 1)
 (:time 6.613306 :buffer-idx 363 :amp -20.5 :transp 1.1070023 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.64155066 :out1
        0 :out2 1)
 (:time 6.618233 :buffer-idx 363 :amp -27.5 :transp -26.421986 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26991534 :out1
        0 :out2 1)
 (:time 6.651621 :buffer-idx 363 :amp -21.5 :transp -26.139225 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5616572 :out1 0
        :out2 1)
 (:time 6.661294 :buffer-idx 362 :amp -27.0 :transp -12.759648 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26379812 :out1
        0 :out2 1)
 (:time 6.686481 :buffer-idx 363 :amp -27.5 :transp -25.84399 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6343646 :out1 0
        :out2 1)
 (:time 6.7022276 :buffer-idx 362 :amp -17.0 :transp 17.678299 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.69694686 :out1
        0 :out2 1)
 (:time 6.7221484 :buffer-idx 363 :amp -19.5 :transp -25.541924 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7546241 :out1 0
        :out2 1)
 (:time 6.7557282 :buffer-idx 361 :amp -24.0 :transp -11.860865 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14674592 :out1
        0 :out2 1)
 (:time 6.759397 :buffer-idx 361 :amp -27.5 :transp -25.226463 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38782752 :out1
        0 :out2 1)
 (:time 6.795918 :buffer-idx 361 :amp -18.5 :transp -24.917164 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.52014065 :out1
        0 :out2 1)
 (:time 6.81777 :buffer-idx 362 :amp -28.0 :transp -11.270376 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6333525 :out1 0
        :out2 1)
 (:time 6.8330307 :buffer-idx 362 :amp -29.5 :transp -24.602854 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7520069 :out1 0
        :out2 1)
 (:time 6.871677 :buffer-idx 362 :amp -19.5 :transp -24.275558 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16730964 :out1
        0 :out2 1)
 (:time 6.876964 :buffer-idx 363 :amp -27.0 :transp -10.706993 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51632345 :out1
        0 :out2 1)
 (:time 6.9024296 :buffer-idx 363 :amp -24.5 :transp 4.196642 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.32239842 :out1
        0 :out2 1)
 (:time 6.9102187 :buffer-idx 361 :amp -28.5 :transp -23.949144 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.28569043 :out1
        0 :out2 1)
 (:time 6.9497786 :buffer-idx 363 :amp -27.5 :transp -23.614107 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23956203 :out1
        0 :out2 1)
 (:time 6.977299 :buffer-idx 361 :amp -23.0 :transp -9.752047 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.99400294 :out1
        0 :out2 1)
 (:time 6.9929004 :buffer-idx 361 :amp -18.5 :transp -23.248909 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.25627697 :out1
        0 :out2 1)
 (:time 7.0329947 :buffer-idx 362 :amp -28.5 :transp -22.909346 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.99571085 :out1
        0 :out2 1)
 (:time 7.0520344 :buffer-idx 362 :amp -26.0 :transp -9.0407505 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18044996 :out1
        0 :out2 1)
 (:time 7.0724063 :buffer-idx 363 :amp -24.5 :transp -22.575565 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.54377735 :out1
        0 :out2 1)
 (:time 7.0871787 :buffer-idx 363 :amp -20.5 :transp 6.1709156 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.036993146 :out1
        0 :out2 1)
 (:time 7.113944 :buffer-idx 361 :amp -23.5 :transp -22.223782 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08939338 :out1
        0 :out2 1)
 (:time 7.150122 :buffer-idx 362 :amp -29.0 :transp -8.107197 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.58155835 :out1
        0 :out2 1)
 (:time 7.1528535 :buffer-idx 362 :amp -21.5 :transp -21.894257 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.67556393 :out1
        0 :out2 1)
 (:time 7.20424 :buffer-idx 363 :amp -23.5 :transp -21.45906 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13109899 :out1
        0 :out2 1)
 (:time 7.2536926 :buffer-idx 363 :amp -18.5 :transp -21.040241 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.015972495 :out1
        0 :out2 1)
 (:time 7.2603054 :buffer-idx 362 :amp -20.0 :transp -7.0585175 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13943887 :out1
        0 :out2 1)
 (:time 7.302855 :buffer-idx 361 :amp -27.5 :transp -20.623886 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20297015 :out1
        0 :out2 1)
 (:time 7.339852 :buffer-idx 362 :amp -19.0 :transp -6.3014297 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17074811 :out1
        0 :out2 1)
 (:time 7.361999 :buffer-idx 362 :amp -20.5 :transp -20.12299 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5988127 :out1 0
        :out2 1)
 (:time 7.4146547 :buffer-idx 362 :amp -26.5 :transp -19.677046 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5756817 :out1 0
        :out2 1)
 (:time 7.450581 :buffer-idx 363 :amp -24.0 :transp -5.2475567 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.93931735 :out1
        0 :out2 1)
 (:time 7.4571466 :buffer-idx 361 :amp -21.5 :transp -19.317179 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3952831 :out1 0
        :out2 1)
 (:time 7.4843054 :buffer-idx 361 :amp -23.0 :transp 27.052982 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4481647 :out1 0
        :out2 1)
 (:time 7.515243 :buffer-idx 363 :amp -21.5 :transp -18.825157 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47935927 :out1
        0 :out2 1)
 (:time 7.5629272 :buffer-idx 363 :amp -24.5 :transp 11.254875 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.30903935 :out1
        0 :out2 1)
 (:time 7.573776 :buffer-idx 363 :amp -23.5 :transp -18.329441 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.53657365 :out1
        0 :out2 1)
 (:time 7.6212535 :buffer-idx 363 :amp -28.5 :transp -17.927345 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8848305 :out1 0
        :out2 1)
 (:time 7.671586 :buffer-idx 362 :amp -23.0 :transp -3.1441307 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.62060094 :out1
        0 :out2 1)
 (:time 7.6968384 :buffer-idx 361 :amp -19.5 :transp -17.287216 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37082064 :out1
        0 :out2 1)
 (:time 7.754728 :buffer-idx 361 :amp -20.5 :transp -16.796947 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9728395 :out1 0
        :out2 1)
 (:time 7.7732596 :buffer-idx 361 :amp -23.0 :transp -30.0 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07132769 :out1
        0 :out2 1)
 (:time 7.8032594 :buffer-idx 362 :amp -28.0 :transp -29.77411 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49291635 :out1
        0 :out2 1)
 (:time 7.8239064 :buffer-idx 363 :amp -18.5 :transp -16.21107 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17589366 :out1
        0 :out2 1)
 (:time 7.833806 :buffer-idx 361 :amp -23.0 :transp -29.544106 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6666844 :out1 0
        :out2 1)
 (:time 7.864909 :buffer-idx 363 :amp -27.0 :transp -29.30991 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20563483 :out1
        0 :out2 1)
 (:time 7.8959737 :buffer-idx 361 :amp -28.0 :transp -29.076006 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9095954 :out1 0
        :out2 1)
 (:time 7.905388 :buffer-idx 363 :amp -22.5 :transp -15.520996 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5262562 :out1 0
        :out2 1)
 (:time 7.912992 :buffer-idx 363 :amp -27.0 :transp -0.8465309 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.57247615 :out1
        0 :out2 1)
 (:time 7.9278245 :buffer-idx 361 :amp -26.0 :transp -28.836178 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.50449085 :out1
        0 :out2 1)
 (:time 7.9595876 :buffer-idx 361 :amp -31.0 :transp -28.597013 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24690211 :out1
        0 :out2 1)
 (:time 7.9755073 :buffer-idx 361 :amp -21.5 :transp -14.92715 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7607446 :out1 0
        :out2 1)
 (:time 7.993144 :buffer-idx 361 :amp -29.0 :transp -28.344345 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.057552457 :out1
        0 :out2 1)
 (:time 8.02523 :buffer-idx 361 :amp -21.0 :transp -28.102747 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77388215 :out1
        0 :out2 1)
 (:time 8.048882 :buffer-idx 362 :amp -28.5 :transp -14.305741 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.83150995 :out1
        0 :out2 1)
 (:time 8.059593 :buffer-idx 361 :amp -28.0 :transp -27.844004 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38726854 :out1
        0 :out2 1)
 (:time 8.095375 :buffer-idx 363 :amp -28.0 :transp -27.57458 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5955092 :out1 0
        :out2 1)
 (:time 8.10441 :buffer-idx 362 :amp -22.5 :transp 17.041279 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.713452 :out1 0
        :out2 1)
 (:time 8.128891 :buffer-idx 362 :amp -24.0 :transp -27.322218 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49576783 :out1
        0 :out2 1)
 (:time 8.154317 :buffer-idx 362 :amp -29.5 :transp -13.4128 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2468003 :out1 0
        :out2 1)
 (:time 8.16648 :buffer-idx 362 :amp -21.0 :transp -27.039185 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5074004 :out1 0
        :out2 1)
 (:time 8.190008 :buffer-idx 362 :amp -27.0 :transp 1.7899876 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.61555815 :out1
        0 :out2 1)
 (:time 8.200435 :buffer-idx 363 :amp -29.0 :transp -26.78352 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.40605402 :out1
        0 :out2 1)
 (:time 8.236522 :buffer-idx 363 :amp -30.0 :transp -26.511793 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3934139 :out1 0
        :out2 1)
 (:time 8.267657 :buffer-idx 362 :amp -23.5 :transp -12.452917 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.052281976 :out1
        0 :out2 1)
 (:time 8.274794 :buffer-idx 363 :amp -25.0 :transp -26.223618 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.92627215 :out1
        0 :out2 1)
 (:time 8.314294 :buffer-idx 363 :amp -24.0 :transp -25.926195 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20208812 :out1
        0 :out2 1)
 (:time 8.35711 :buffer-idx 362 :amp -32.0 :transp -25.603807 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.047313333 :out1
        0 :out2 1)
 (:time 8.367379 :buffer-idx 361 :amp -20.5 :transp -11.608368 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6570045 :out1 0
        :out2 1)
 (:time 8.399258 :buffer-idx 363 :amp -28.0 :transp -25.286448 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37249815 :out1
        0 :out2 1)
 (:time 8.43939 :buffer-idx 361 :amp -27.0 :transp -24.984264 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7493342 :out1 0
        :out2 1)
 (:time 8.477574 :buffer-idx 363 :amp -24.0 :transp -24.69675 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5502312 :out1 0
        :out2 1)
 (:time 8.503301 :buffer-idx 361 :amp -25.5 :transp -10.457237 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.143031 :out1 0
        :out2 1)
 (:time 8.525416 :buffer-idx 362 :amp -32.0 :transp -24.336517 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22770476 :out1
        0 :out2 1)
 (:time 8.547809 :buffer-idx 361 :amp -24.0 :transp 5.1953754 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5850977 :out1 0
        :out2 1)
 (:time 8.564631 :buffer-idx 363 :amp -29.0 :transp -24.041245 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4826883 :out1 0
        :out2 1)
 (:time 8.582789 :buffer-idx 361 :amp -24.5 :transp -9.784044 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14622748 :out1
        0 :out2 1)
 (:time 8.602963 :buffer-idx 363 :amp -31.0 :transp -23.752615 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7575338 :out1 0
        :out2 1)
 (:time 8.653867 :buffer-idx 362 :amp -25.0 :transp -23.369326 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49709463 :out1
        0 :out2 1)
 (:time 8.659084 :buffer-idx 363 :amp -20.5 :transp -9.1378975 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9513229 :out1 0
        :out2 1)
 (:time 8.701378 :buffer-idx 361 :amp -21.0 :transp -23.011585 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37674177 :out1
        0 :out2 1)
 (:time 8.747036 :buffer-idx 361 :amp -28.0 :transp -22.6678 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8357694 :out1 0
        :out2 1)
 (:time 8.758726 :buffer-idx 363 :amp -20.0 :transp 7.202795 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8319278 :out1 0
        :out2 1)
 (:time 8.789642 :buffer-idx 363 :amp -22.5 :transp -8.032188 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0017293692
        :out1 0 :out2 1)
 (:time 8.803709 :buffer-idx 361 :amp -29.0 :transp -22.241068 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.56298506 :out1
        0 :out2 1)
 (:time 8.859727 :buffer-idx 361 :amp -29.0 :transp -21.819271 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5102613 :out1 0
        :out2 1)
 (:time 8.878461 :buffer-idx 361 :amp -24.0 :transp 8.342379 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5750158 :out1 0
        :out2 1)
 (:time 8.9170685 :buffer-idx 361 :amp -25.0 :transp -21.38751 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3434186 :out1 0
        :out2 1)
 (:time 8.921336 :buffer-idx 363 :amp -22.5 :transp -6.9168663 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20675075 :out1
        0 :out2 1)
 (:time 8.96504 :buffer-idx 362 :amp -32.0 :transp -21.026299 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3939607 :out1 0
        :out2 1)
 (:time 9.008037 :buffer-idx 362 :amp -24.0 :transp -20.702553 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9393376 :out1 0
        :out2 1)
 (:time 9.022927 :buffer-idx 363 :amp -18.5 :transp 26.856766 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36838937 :out1
        0 :out2 1)
 (:time 9.043416 :buffer-idx 362 :amp -26.5 :transp -5.882965 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43013322 :out1
        0 :out2 1)
 (:time 9.054737 :buffer-idx 361 :amp -21.0 :transp -20.350918 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.90817976 :out1
        0 :out2 1)
 (:time 9.112572 :buffer-idx 363 :amp -21.0 :transp -19.915436 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.33769715 :out1
        0 :out2 1)
 (:time 9.145197 :buffer-idx 362 :amp -23.5 :transp -5.020977 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4207697 :out1 0
        :out2 1)
 (:time 9.161317 :buffer-idx 361 :amp -31.0 :transp -19.548405 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21059 :out1 0
        :out2 1)
 (:time 9.216101 :buffer-idx 363 :amp -27.0 :transp 11.555891 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.81423223 :out1
        0 :out2 1)
 (:time 9.222964 :buffer-idx 361 :amp -28.0 :transp -19.084219 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.98222375 :out1
        0 :out2 1)
 (:time 9.26942 :buffer-idx 363 :amp -30.0 :transp -18.734425 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37187076 :out1
        0 :out2 1)
 (:time 9.314349 :buffer-idx 362 :amp -25.5 :transp -3.588417 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0765059 :out1 0
        :out2 1)
 (:time 9.320783 :buffer-idx 362 :amp -25.0 :transp -18.347683 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16152227 :out1
        0 :out2 1)
 (:time 9.366958 :buffer-idx 361 :amp -27.5 :transp -31.7 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77614224 :out1
        0 :out2 1)
 (:time 9.368043 :buffer-idx 361 :amp -30.0 :transp -17.991829 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.92873085 :out1
        0 :out2 1)
 (:time 9.396957 :buffer-idx 361 :amp -26.5 :transp -31.499329 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.02985549 :out1
        0 :out2 1)
 (:time 9.427266 :buffer-idx 363 :amp -28.5 :transp -31.29659 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45469463 :out1
        0 :out2 1)
 (:time 9.441312 :buffer-idx 361 :amp -30.0 :transp -17.440136 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.97384775 :out1
        0 :out2 1)
 (:time 9.458249 :buffer-idx 361 :amp -32.5 :transp -31.089346 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7553083 :out1 0
        :out2 1)
 (:time 9.489331 :buffer-idx 362 :amp -31.5 :transp -30.881433 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8557546 :out1 0
        :out2 1)
 (:time 9.499658 :buffer-idx 361 :amp -22.0 :transp -17.000813 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47629404 :out1
        0 :out2 1)
 (:time 9.521336 :buffer-idx 363 :amp -29.5 :transp -30.667356 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.067496896 :out1
        0 :out2 1)
 (:time 9.552585 :buffer-idx 361 :amp -27.5 :transp -30.458326 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3183415 :out1 0
        :out2 1)
 (:time 9.557674 :buffer-idx 363 :amp -26.5 :transp -1.5276794 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49946213 :out1
        0 :out2 1)
 (:time 9.584428 :buffer-idx 361 :amp -25.5 :transp -30.245323 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8752098 :out1 0
        :out2 1)
 (:time 9.594198 :buffer-idx 362 :amp -22.0 :transp -16.28896 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.39235544 :out1
        0 :out2 1)
 (:time 9.61832 :buffer-idx 361 :amp -27.5 :transp -30.018612 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.010777831 :out1
        0 :out2 1)
 (:time 9.645468 :buffer-idx 363 :amp -27.0 :transp 15.64242 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8589014 :out1 0
        :out2 1)
 (:time 9.650445 :buffer-idx 362 :amp -28.5 :transp -29.803734 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17103708 :out1
        0 :out2 1)
 (:time 9.6688595 :buffer-idx 363 :amp -28.0 :transp -15.726781 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37143242 :out1
        0 :out2 1)
 (:time 9.684612 :buffer-idx 361 :amp -31.5 :transp -29.575184 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37353218 :out1
        0 :out2 1)
 (:time 9.712093 :buffer-idx 361 :amp -23.5 :transp -0.21989441 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.668448 :out1 0
        :out2 1)
 (:time 9.717563 :buffer-idx 363 :amp -33.5 :transp -29.354774 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10559785 :out1
        0 :out2 1)
 (:time 9.743813 :buffer-idx 362 :amp -21.0 :transp -15.162412 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.02912128 :out1
        0 :out2 1)
 (:time 9.751646 :buffer-idx 362 :amp -31.5 :transp -29.12679 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8562621 :out1 0
        :out2 1)
 (:time 9.787286 :buffer-idx 363 :amp -26.5 :transp -28.888391 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13174617 :out1
        0 :out2 1)
 (:time 9.82137 :buffer-idx 363 :amp -23.5 :transp -28.660397 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.32202148 :out1
        0 :out2 1)
 (:time 9.844105 :buffer-idx 362 :amp -29.0 :transp -14.407245 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4397092 :out1 0
        :out2 1)
 (:time 9.85859 :buffer-idx 363 :amp -31.5 :transp -28.41143 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43348432 :out1
        0 :out2 1)
 (:time 9.865055 :buffer-idx 363 :amp -29.5 :transp 1.0755424 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3098824 :out1 0
        :out2 1)
 (:time 9.894904 :buffer-idx 361 :amp -32.5 :transp -28.168524 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9590349 :out1 0
        :out2 1)
 (:time 9.926151 :buffer-idx 362 :amp -23.0 :transp -13.789459 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1773001 :out1 0
        :out2 1)
 (:time 9.934487 :buffer-idx 361 :amp -33.5 :transp -27.90375 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.008950472 :out1
        0 :out2 1)
 (:time 9.96593 :buffer-idx 363 :amp -23.0 :transp 18.692434 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2339114 :out1 0
        :out2 1)
 (:time 9.97417 :buffer-idx 362 :amp -31.5 :transp -27.638311 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9590297 :out1 0
        :out2 1)
 (:time 9.983819 :buffer-idx 362 :amp -28.5 :transp 2.0813637 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8348315 :out1 0
        :out2 1)
 (:time 10.010461 :buffer-idx 362 :amp -24.5 :transp -27.395554 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7953297 :out1 0
        :out2 1)
 (:time 10.018473 :buffer-idx 363 :amp -31.0 :transp -13.094315 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29728377 :out1
        0 :out2 1)
 (:time 10.050836 :buffer-idx 361 :amp -27.5 :transp -27.125486 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9373454 :out1 0
        :out2 1)
 (:time 10.091488 :buffer-idx 362 :amp -24.5 :transp -26.853558 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9808668 :out1 0
        :out2 1)
 (:time 10.114877 :buffer-idx 363 :amp -24.0 :transp -12.368425 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.72541416 :out1
        0 :out2 1)
 (:time 10.136245 :buffer-idx 363 :amp -29.5 :transp -26.554174 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17006516 :out1
        0 :out2 1)
 (:time 10.1772175 :buffer-idx 363 :amp -29.5 :transp -26.280111 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6725118 :out1
        0 :out2 1)
 (:time 10.188359 :buffer-idx 362 :amp -23.0 :transp -11.8151245 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.32412124
        :out1 0 :out2 1)
 (:time 10.206446 :buffer-idx 361 :amp -23.5 :transp 3.9668007 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6180514 :out1 0
        :out2 1)
 (:time 10.219408 :buffer-idx 361 :amp -26.5 :transp -25.997894 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2947166 :out1 0
        :out2 1)
 (:time 10.259496 :buffer-idx 361 :amp -27.5 :transp -25.729746 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4699515 :out1 0
        :out2 1)
 (:time 10.296058 :buffer-idx 361 :amp -32.0 :transp -11.004194 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49989855 :out1
        0 :out2 1)
 (:time 10.3095255 :buffer-idx 361 :amp -23.5 :transp -25.395092 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5462204 :out1
        0 :out2 1)
 (:time 10.353255 :buffer-idx 362 :amp -31.5 :transp -25.102581 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5479344 :out1 0
        :out2 1)
 (:time 10.370884 :buffer-idx 362 :amp -32.0 :transp -10.440777 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.034461856 :out1
        0 :out2 1)
 (:time 10.39748 :buffer-idx 362 :amp -32.5 :transp -24.806751 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.78743744 :out1
        0 :out2 1)
 (:time 10.411537 :buffer-idx 361 :amp -18.5 :transp 5.7037277 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7844546 :out1 0
        :out2 1)
 (:time 10.445044 :buffer-idx 363 :amp -29.5 :transp -24.488596 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.27646327 :out1
        0 :out2 1)
 (:time 10.49137 :buffer-idx 362 :amp -28.5 :transp -24.178717 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77985156 :out1
        0 :out2 1)
 (:time 10.526085 :buffer-idx 362 :amp -25.0 :transp -9.272173 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16465628 :out1
        0 :out2 1)
 (:time 10.534721 :buffer-idx 361 :amp -34.5 :transp -23.888737 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8386711 :out1 0
        :out2 1)
 (:time 10.582637 :buffer-idx 363 :amp -34.5 :transp -23.568228 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1670481 :out1 0
        :out2 1)
 (:time 10.593701 :buffer-idx 363 :amp -21.0 :transp -8.763041 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13290155 :out1
        0 :out2 1)
 (:time 10.624148 :buffer-idx 361 :amp -32.5 :transp -23.29055 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.19504166 :out1
        0 :out2 1)
 (:time 10.665543 :buffer-idx 361 :amp -21.0 :transp -8.222103 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24922097 :out1
        0 :out2 1)
 (:time 10.682824 :buffer-idx 363 :amp -23.5 :transp -22.898067 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5067209 :out1 0
        :out2 1)
 (:time 10.729794 :buffer-idx 363 :amp -24.5 :transp -22.583885 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.89710426 :out1
        0 :out2 1)
 (:time 10.782481 :buffer-idx 362 :amp -30.5 :transp -22.23145 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.333534 :out1 0
        :out2 1)
 (:time 10.835766 :buffer-idx 363 :amp -33.5 :transp -21.875027 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9072763 :out1 0
        :out2 1)
 (:time 10.860978 :buffer-idx 362 :amp -29.0 :transp -6.750538 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.52721906 :out1
        0 :out2 1)
 (:time 10.900372 :buffer-idx 362 :amp -31.5 :transp -21.442875 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44167066 :out1
        0 :out2 1)
 (:time 10.911395 :buffer-idx 361 :amp -29.0 :transp 27.690962 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.28011072 :out1
        0 :out2 1)
 (:time 10.946523 :buffer-idx 361 :amp -26.5 :transp -21.134163 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10100758 :out1
        0 :out2 1)
 (:time 11.008882 :buffer-idx 363 :amp -24.5 :transp -20.717041 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7428857 :out1 0
        :out2 1)
 (:time 11.048116 :buffer-idx 363 :amp -24.5 :transp 11.094959 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.025400639 :out1
        0 :out2 1)
 (:time 11.062918 :buffer-idx 362 :amp -30.5 :transp -20.35559 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08129072 :out1
        0 :out2 1)
 (:time 11.084374 :buffer-idx 361 :amp -28.0 :transp -5.0684395 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8685628 :out1 0
        :out2 1)
 (:time 11.122459 :buffer-idx 362 :amp -26.5 :transp -19.95731 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7620498 :out1 0
        :out2 1)
 (:time 11.199481 :buffer-idx 361 :amp -21.5 :transp 12.376881 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18360186 :out1
        0 :out2 1)
 (:time 11.201426 :buffer-idx 363 :amp -34.5 :transp -19.429096 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15615726 :out1
        0 :out2 1)
 (:time 11.261252 :buffer-idx 363 :amp -30.5 :transp -19.028913 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15801919 :out1
        0 :out2 1)
 (:time 11.293073 :buffer-idx 362 :amp -28.0 :transp -3.497015 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3986528 :out1 0
        :out2 1)
 (:time 11.329537 :buffer-idx 361 :amp -31.5 :transp -18.572147 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0716753 :out1 0
        :out2 1)
 (:time 11.382843 :buffer-idx 361 :amp -27.0 :transp -2.8210773 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6374543 :out1 0
        :out2 1)
 (:time 11.409657 :buffer-idx 361 :amp -23.5 :transp -18.036224 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.56830204 :out1
        0 :out2 1)
 (:time 11.475757 :buffer-idx 362 :amp -25.5 :transp -17.594078 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3384621 :out1 0
        :out2 1)
 (:time 11.50639 :buffer-idx 362 :amp -22.5 :transp 14.976101 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.30644548 :out1
        0 :out2 1)
 (:time 11.549588 :buffer-idx 361 :amp -32.0 :transp -1.5655384 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29122567 :out1
        0 :out2 1)
 (:time 11.561926 :buffer-idx 361 :amp -28.5 :transp -17.017685 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59590983 :out1
        0 :out2 1)
 (:time 11.650411 :buffer-idx 363 :amp -25.5 :transp -16.4258 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8519423 :out1 0
        :out2 1)
 (:time 11.670954 :buffer-idx 362 :amp -23.0 :transp -0.65169907 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2514863 :out1
        0 :out2 1)
 (:time 11.709653 :buffer-idx 362 :amp -29.5 :transp 16.697552 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5917932 :out1 0
        :out2 1)
 (:time 11.7576 :buffer-idx 362 :amp -33.5 :transp -15.708807 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08834386 :out1
        0 :out2 1)
 (:time 11.823409 :buffer-idx 362 :amp -23.5 :transp -15.268602 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.91502714 :out1
        0 :out2 1)
 (:time 11.893904 :buffer-idx 362 :amp -22.0 :transp 1.0270348 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8113389 :out1 0
        :out2 1)
 (:time 11.919676 :buffer-idx 362 :amp -28.5 :transp -14.624668 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4444449 :out1 0
        :out2 1)
 (:time 12.007509 :buffer-idx 363 :amp -27.0 :transp 1.8824501 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44941974 :out1
        0 :out2 1)
 (:time 12.041335 :buffer-idx 361 :amp -34.5 :transp -13.810879 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.483572 :out1 0
        :out2 1)
 (:time 12.106817 :buffer-idx 362 :amp -27.5 :transp -13.37286 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43304968 :out1
        0 :out2 1)
 (:time 12.139309 :buffer-idx 362 :amp -30.0 :transp 2.8748512 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.021577 :out1 0
        :out2 1)
 (:time 12.21042 :buffer-idx 363 :amp -24.5 :transp -12.679857 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0897063 :out1 0
        :out2 1)
 (:time 12.260754 :buffer-idx 363 :amp -30.0 :transp 3.7892876 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.60497105 :out1
        0 :out2 1)
 (:time 12.304822 :buffer-idx 363 :amp -28.5 :transp -12.048395 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15958357 :out1
        0 :out2 1)
 (:time 12.382284 :buffer-idx 363 :amp -24.5 :transp -11.530241 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07768905 :out1
        0 :out2 1)
 (:time 12.460691 :buffer-idx 363 :amp -23.5 :transp -11.00577 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6574727 :out1 0
        :out2 1)
 (:time 12.52625 :buffer-idx 361 :amp -23.5 :transp -10.567242 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6292901 :out1 0
        :out2 1)
 (:time 12.657899 :buffer-idx 363 :amp -29.5 :transp -9.68663 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42970514 :out1
        0 :out2 1)
 (:time 12.670807 :buffer-idx 362 :amp -28.0 :transp 6.87685 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.055156946 :out1
        0 :out2 1)
 (:time 12.715999 :buffer-idx 361 :amp -27.5 :transp 25.22036 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.987797 :out1 0
        :out2 1)
 (:time 12.758523 :buffer-idx 361 :amp -31.5 :transp -9.013552 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16112411 :out1
        0 :out2 1)
 (:time 12.945892 :buffer-idx 361 :amp -25.5 :transp -7.7602215 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3820001 :out1 0
        :out2 1)
 (:time 12.955106 :buffer-idx 362 :amp -28.0 :transp 9.017525 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5605086 :out1 0
        :out2 1)
 (:time 13.091542 :buffer-idx 362 :amp -27.5 :transp -6.7859573 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.471655 :out1 0
        :out2 1)
 (:time 13.198413 :buffer-idx 363 :amp -23.5 :transp -6.0710926 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.65403736 :out1
        0 :out2 1)
 (:time 13.373898 :buffer-idx 363 :amp -24.5 :transp -4.8972588 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6993326 :out1 0
        :out2 1)
 (:time 13.583323 :buffer-idx 363 :amp -31.5 :transp -3.4964008 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0011188984
        :out1 0 :out2 1)
 (:time 13.781138 :buffer-idx 361 :amp -31.0 :transp 15.237267 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2985009 :out1 0
        :out2 1)
 (:time 13.861072 :buffer-idx 363 :amp -31.5 :transp -1.6385117 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20172358 :out1
        0 :out2 1)
 (:time 13.976894 :buffer-idx 362 :amp -28.0 :transp 16.711243 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.60742664 :out1
        0 :out2 1)
 (:time 13.988267 :buffer-idx 361 :amp -25.5 :transp -0.78769493 :start 0 :end
        0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.69618285
        :out1 0 :out2 1)
 (:time 14.326731 :buffer-idx 363 :amp -34.5 :transp 1.4763184 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.84767103 :out1
        0 :out2 1)
 (:time 14.73504 :buffer-idx 361 :amp -27.0 :transp 22.419811 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45379698 :out1
        0 :out2 1)
 (:time 14.756125 :buffer-idx 363 :amp -32.5 :transp 4.3485756 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4538021 :out1 0
        :out2 1)
 (:time 14.922933 :buffer-idx 362 :amp -33.5 :transp 5.4643593 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08951843 :out1
        0 :out2 1)
 (:time 15.058363 :buffer-idx 362 :amp -24.0 :transp 24.854324 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9872401 :out1 0
        :out2 1)
 (:time 15.14917 :buffer-idx 361 :amp -24.5 :transp 6.9776764 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23179412 :out1
        0 :out2 1)
 (:time 15.38891 :buffer-idx 363 :amp -30.5 :transp 8.581322 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9562222 :out1 0
        :out2 1)
 (:time 15.446611 :buffer-idx 362 :amp -28.0 :transp 27.77771 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17807245 :out1
        0 :out2 1)
 (:time 15.5480385 :buffer-idx 361 :amp -31.5 :transp 9.6457405 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8964753 :out1 0
        :out2 1)
 (:time 16.523932 :buffer-idx 362 :amp -26.5 :transp 16.173569 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.856002 :out1 0
        :out2 1)
 (:time 17.106852 :buffer-idx 362 :amp -29.5 :transp 20.072773 :start 0 :end 0
        :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42790306 :out1
        0 :out2 1))
'((:time 0.0 :buffer-idx 363 :amp -25.5 :transp -21.5 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6022301 :out1 0 :out2 1)
  (:time 0.0 :buffer-idx 363 :amp -10.0 :transp -19.8 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7521955 :out1 0 :out2 1)
  (:time 0.0 :buffer-idx 362 :amp -13.5 :transp -18.1 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.53056526 :out1 0 :out2 1)
  (:time 0.0 :buffer-idx 361 :amp -19.0 :transp -16.4 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37542665 :out1 0 :out2 1)
  (:time 0.0 :buffer-idx 361 :amp -10.5 :transp -14.7 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.62394845 :out1 0 :out2 1)
  (:time 0.0 :buffer-idx 363 :amp -7.0 :transp -13.0 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8620223 :out1 0 :out2 1)
  (:time 0.011609977 :buffer-idx 361 :amp -21.0 :transp -26.6 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8198546 :out1 0 :out2 1)
  (:time 0.011609977 :buffer-idx 363 :amp -19.5 :transp -24.900002 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.64579475 :out1 0 :out2 1)
  (:time 0.011609977 :buffer-idx 363 :amp -23.0 :transp -23.2 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.390993 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 363 :amp -19.5 :transp -21.199228 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.499843 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 363 :amp -15.0 :transp -19.444344 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5969442 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 361 :amp -13.5 :transp -17.679935 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26509452 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 361 :amp -16.0 :transp -15.904479 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1492672 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 362 :amp -6.5 :transp -14.116256 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26395166 :out1 0 :out2 1)
  (:time 0.03 :buffer-idx 361 :amp -4.0 :transp -12.31332 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5546813 :out1 0 :out2 1)
  (:time 0.041609976 :buffer-idx 363 :amp -19.0 :transp -26.419228 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.932145 :out1 0 :out2 1)
  (:time 0.041609976 :buffer-idx 362 :amp -21.5 :transp -24.685585 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24176502 :out1 0 :out2 1)
  (:time 0.041609976 :buffer-idx 361 :amp -23.0 :transp -22.94592 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08752632 :out1 0 :out2 1)
  (:time 0.060766052 :buffer-idx 362 :amp -11.0 :transp -15.396306 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38416576 :out1 0 :out2 1)
  (:time 0.060868833 :buffer-idx 361 :amp -18.5 :transp -20.889746 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47977388 :out1 0 :out2 1)
  (:time 0.060902588 :buffer-idx 361 :amp -17.0 :transp -19.077988 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.273669 :out1 0 :out2 1)
  (:time 0.061308943 :buffer-idx 361 :amp -17.5 :transp -17.24154 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5435488 :out1 0 :out2 1)
  (:time 0.061464056 :buffer-idx 363 :amp -5.0 :transp -11.593129 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.027734995 :out1 0 :out2 1)
  (:time 0.061962597 :buffer-idx 362 :amp -12.5 :transp -13.494324 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12338972 :out1 0 :out2 1)
  (:time 0.07184288 :buffer-idx 362 :amp -16.0 :transp -26.237053 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12273097
   :out1 0 :out2 1)
  (:time 0.07220738 :buffer-idx 362 :amp -20.0 :transp -22.68678 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2131393 :out1 0
   :out2 1)
  (:time 0.07221965 :buffer-idx 362 :amp -15.5 :transp -24.46681 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4021989 :out1 0
   :out2 1)
  (:time 0.09241777 :buffer-idx 362 :amp -21.5 :transp -20.573444 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8838886 :out1
   0 :out2 1)
  (:time 0.09278592 :buffer-idx 361 :amp -18.0 :transp -18.700003 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31161952
   :out1 0 :out2 1)
  (:time 0.09299729 :buffer-idx 362 :amp -12.5 :transp -16.797834 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17826879
   :out1 0 :out2 1)
  (:time 0.09334423 :buffer-idx 362 :amp -15.0 :transp -14.858201 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.026090503
   :out1 0 :out2 1)
  (:time 0.09641427 :buffer-idx 361 :amp -14.5 :transp -12.823959 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36434126
   :out1 0 :out2 1)
  (:time 0.09681688 :buffer-idx 363 :amp -4.0 :transp -10.783926 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07085216 :out1
   0 :out2 1)
  (:time 0.102788776 :buffer-idx 363 :amp -17.5 :transp -24.248325 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.755991 :out1
   0 :out2 1)
  (:time 0.10286874 :buffer-idx 363 :amp -22.0 :transp -26.050098 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16968107
   :out1 0 :out2 1)
  (:time 0.10330558 :buffer-idx 361 :amp -18.0 :transp -22.4234 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5394167 :out1 0
   :out2 1)
  (:time 0.12421577 :buffer-idx 363 :amp -16.5 :transp -20.254646 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.74994314
   :out1 0 :out2 1)
  (:time 0.12493947 :buffer-idx 361 :amp -18.0 :transp -18.318817 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9949373 :out1
   0 :out2 1)
  (:time 0.12567946 :buffer-idx 363 :amp -11.5 :transp -16.340212 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.96340954
   :out1 0 :out2 1)
  (:time 0.12591629 :buffer-idx 362 :amp -16.0 :transp -14.320197 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.67338836
   :out1 0 :out2 1)
  (:time 0.13234994 :buffer-idx 362 :amp -5.0 :transp -9.970598 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.074964404 :out1
   0 :out2 1)
  (:time 0.13279304 :buffer-idx 361 :amp -11.5 :transp -12.1160965 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.09886539
   :out1 0 :out2 1)
  (:time 0.1339951 :buffer-idx 362 :amp -19.5 :transp -24.025286 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6406826 :out1 0
   :out2 1)
  (:time 0.1343761 :buffer-idx 363 :amp -23.0 :transp -25.860243 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6852019 :out1 0
   :out2 1)
  (:time 0.13494056 :buffer-idx 361 :amp -21.0 :transp -22.155474 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.592283 :out1
   0 :out2 1)
  (:time 0.15740849 :buffer-idx 361 :amp -24.5 :transp -19.921865 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59001064
   :out1 0 :out2 1)
  (:time 0.15976644 :buffer-idx 362 :amp -10.0 :transp -17.905935 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.76752675
   :out1 0 :out2 1)
  (:time 0.1606944 :buffer-idx 363 :amp -16.5 :transp -15.849926 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38450277 :out1
   0 :out2 1)
  (:time 0.16335309 :buffer-idx 362 :amp -15.0 :transp -13.70184 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.94065773 :out1
   0 :out2 1)
  (:time 0.16604953 :buffer-idx 363 :amp -23.5 :transp -23.796185 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49647677
   :out1 0 :out2 1)
  (:time 0.16650766 :buffer-idx 362 :amp -18.0 :transp -25.666626 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37792313
   :out1 0 :out2 1)
  (:time 0.16812314 :buffer-idx 363 :amp -12.5 :transp -11.428638 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.99317956
   :out1 0 :out2 1)
  (:time 0.16821934 :buffer-idx 363 :amp -21.0 :transp -21.873625 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08019781
   :out1 0 :out2 1)
  (:time 0.16906406 :buffer-idx 361 :amp -8.0 :transp -9.130237 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.114133835 :out1
   0 :out2 1)
  (:time 0.18980618 :buffer-idx 362 :amp -17.5 :transp -19.597055 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.81225395
   :out1 0 :out2 1)
  (:time 0.1957328 :buffer-idx 362 :amp -14.0 :transp -17.479548 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9223485 :out1 0
   :out2 1)
  (:time 0.19860433 :buffer-idx 362 :amp -11.5 :transp -15.319104 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.67759 :out1 0
   :out2 1)
  (:time 0.1988751 :buffer-idx 363 :amp -18.5 :transp -23.561573 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.56347907 :out1
   0 :out2 1)
  (:time 0.19913763 :buffer-idx 362 :amp -22.0 :transp -25.470005 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.62363815
   :out1 0 :out2 1)
  (:time 0.20026548 :buffer-idx 363 :amp -21.0 :transp -21.602217 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9523891 :out1
   0 :out2 1)
  (:time 0.20077695 :buffer-idx 361 :amp -15.0 :transp -13.083698 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.116876125
   :out1 0 :out2 1)
  (:time 0.20572919 :buffer-idx 362 :amp -5.5 :transp -10.696896 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8148197 :out1 0
   :out2 1)
  (:time 0.21528295 :buffer-idx 361 :amp -11.0 :transp -8.072317 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7918682 :out1 0
   :out2 1)
  (:time 0.22579604 :buffer-idx 361 :amp -23.5 :transp -19.23623 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2191261 :out1 0
   :out2 1)
  (:time 0.23143 :buffer-idx 362 :amp -23.5 :transp -23.328896 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7263621 :out1 0
   :out2 1)
  (:time 0.23188643 :buffer-idx 363 :amp -19.0 :transp -25.272669 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6276592 :out1
   0 :out2 1)
  (:time 0.23302498 :buffer-idx 361 :amp -19.0 :transp -17.037441 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.35450327
   :out1 0 :out2 1)
  (:time 0.23331814 :buffer-idx 362 :amp -18.5 :transp -14.833035 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.41683865
   :out1 0 :out2 1)
  (:time 0.23437874 :buffer-idx 363 :amp -19.0 :transp -21.3133 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7637433 :out1 0
   :out2 1)
  (:time 0.24000528 :buffer-idx 363 :amp -10.0 :transp -12.43575 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47647524 :out1
   0 :out2 1)
  (:time 0.2431841 :buffer-idx 362 :amp -3.5 :transp -9.968094 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.014532089 :out1
   0 :out2 1)
  (:time 0.25927734 :buffer-idx 361 :amp -13.0 :transp -7.065315 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.94976914 :out1
   0 :out2 1)
  (:time 0.261567 :buffer-idx 362 :amp -14.5 :transp -18.877602 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3512659 :out1 0
   :out2 1)
  (:time 0.26378548 :buffer-idx 363 :amp -18.5 :transp -23.097643 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6713375 :out1
   0 :out2 1)
  (:time 0.2643671 :buffer-idx 363 :amp -25.0 :transp -25.07695 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9683083 :out1 0
   :out2 1)
  (:time 0.26865137 :buffer-idx 362 :amp -18.5 :transp -14.338293 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20664883
   :out1 0 :out2 1)
  (:time 0.26965988 :buffer-idx 361 :amp -16.0 :transp -21.014492 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10795188
   :out1 0 :out2 1)
  (:time 0.26982915 :buffer-idx 361 :amp -14.0 :transp -16.60112 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6147232 :out1 0
   :out2 1)
  (:time 0.28133923 :buffer-idx 363 :amp -17.0 :transp -11.753023 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.713804 :out1
   0 :out2 1)
  (:time 0.28469715 :buffer-idx 363 :amp -11.5 :transp -9.160328 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.96062005 :out1
   0 :out2 1)
  (:time 0.29745355 :buffer-idx 363 :amp -15.5 :transp -22.85701 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17315209 :out1
   0 :out2 1)
  (:time 0.2988904 :buffer-idx 362 :amp -16.0 :transp -24.868921 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6480874 :out1 0
   :out2 1)
  (:time 0.30045947 :buffer-idx 361 :amp -19.5 :transp -18.487677 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8045417 :out1
   0 :out2 1)
  (:time 0.30281413 :buffer-idx 362 :amp -15.0 :transp -20.733698 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26773536
   :out1 0 :out2 1)
  (:time 0.3055898 :buffer-idx 363 :amp -18.5 :transp -13.821074 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.594121 :out1 0
   :out2 1)
  (:time 0.30786288 :buffer-idx 362 :amp -9.0 :transp -16.150223 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9969473 :out1 0
   :out2 1)
  (:time 0.3142032 :buffer-idx 362 :amp -12.0 :transp -5.8080983 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45159507 :out1
   0 :out2 1)
  (:time 0.3287915 :buffer-idx 362 :amp -11.0 :transp -10.969238 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59258986 :out1
   0 :out2 1)
  (:time 0.33160344 :buffer-idx 361 :amp -20.5 :transp -22.612932 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42157924
   :out1 0 :out2 1)
  (:time 0.33201763 :buffer-idx 361 :amp -24.0 :transp -24.669304 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12009299
   :out1 0 :out2 1)
  (:time 0.33499154 :buffer-idx 361 :amp -11.5 :transp -8.181693 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59400594 :out1
   0 :out2 1)
  (:time 0.3364252 :buffer-idx 361 :amp -19.0 :transp -20.449036 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24499273 :out1
   0 :out2 1)
  (:time 0.34051806 :buffer-idx 361 :amp -24.5 :transp -18.08606 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36451864 :out1
   0 :out2 1)
  (:time 0.34469834 :buffer-idx 363 :amp -10.0 :transp -15.71353 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.851449 :out1 0
   :out2 1)
  (:time 0.3453086 :buffer-idx 363 :amp -17.5 :transp -13.264923 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.35364068 :out1
   0 :out2 1)
  (:time 0.36469358 :buffer-idx 362 :amp -14.0 :transp -4.6524067 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.40702677
   :out1 0 :out2 1)
  (:time 0.36490798 :buffer-idx 361 :amp -19.0 :transp -24.471115 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14770293
   :out1 0 :out2 1)
  (:time 0.36622626 :buffer-idx 362 :amp -14.5 :transp -22.365475 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38111377
   :out1 0 :out2 1)
  (:time 0.37203857 :buffer-idx 361 :amp -20.0 :transp -20.147413 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.56281483
   :out1 0 :out2 1)
  (:time 0.37585023 :buffer-idx 363 :amp -9.0 :transp -10.191953 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.020828009 :out1
   0 :out2 1)
  (:time 0.38039386 :buffer-idx 361 :amp -10.5 :transp -7.2982497 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5703752 :out1
   0 :out2 1)
  (:time 0.3809982 :buffer-idx 362 :amp -17.5 :transp -17.680218 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6924782 :out1 0
   :out2 1)
  (:time 0.38876805 :buffer-idx 363 :amp -12.5 :transp -12.656396 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4597324 :out1
   0 :out2 1)
  (:time 0.39038306 :buffer-idx 363 :amp -18.0 :transp -15.171928 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44162083
   :out1 0 :out2 1)
  (:time 0.39845884 :buffer-idx 361 :amp -19.0 :transp -24.268946 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.25071204
   :out1 0 :out2 1)
  (:time 0.40161088 :buffer-idx 363 :amp -17.5 :transp -22.112572 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1434114 :out1
   0 :out2 1)
  (:time 0.41157398 :buffer-idx 363 :amp -20.0 :transp -19.812576 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5667219 :out1
   0 :out2 1)
  (:time 0.4234098 :buffer-idx 363 :amp -16.5 :transp -17.25501 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.921067 :out1 0
   :out2 1)
  (:time 0.42770517 :buffer-idx 362 :amp -18.5 :transp -12.111191 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4485885 :out1
   0 :out2 1)
  (:time 0.4296401 :buffer-idx 361 :amp -12.0 :transp -9.303486 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15173376 :out1
   0 :out2 1)
  (:time 0.43076414 :buffer-idx 361 :amp -7.5 :transp -6.318138 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2509352 :out1 0
   :out2 1)
  (:time 0.43547308 :buffer-idx 363 :amp -26.0 :transp -24.045908 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.99068916
   :out1 0 :out2 1)
  (:time 0.435497 :buffer-idx 361 :amp -14.0 :transp -14.637094 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.40991414 :out1
   0 :out2 1)
  (:time 0.43557206 :buffer-idx 362 :amp -13.5 :transp -21.869843 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.060173392
   :out1 0 :out2 1)
  (:time 0.44555733 :buffer-idx 363 :amp -10.0 :transp -2.8014898 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8724551 :out1
   0 :out2 1)
  (:time 0.44992936 :buffer-idx 361 :amp -26.0 :transp -19.487732 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.023724318
   :out1 0 :out2 1)
  (:time 0.4664184 :buffer-idx 361 :amp -20.5 :transp -16.823818 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9158559 :out1 0
   :out2 1)
  (:time 0.46971 :buffer-idx 362 :amp -21.0 :transp -23.839603 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.32375526 :out1
   0 :out2 1)
  (:time 0.47129193 :buffer-idx 363 :amp -20.5 :transp -21.614544 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31526923
   :out1 0 :out2 1)
  (:time 0.48166907 :buffer-idx 361 :amp -11.5 :transp -11.355577 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4322437 :out1
   0 :out2 1)
  (:time 0.4819589 :buffer-idx 362 :amp -12.0 :transp -14.086279 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24126709 :out1
   0 :out2 1)
  (:time 0.48404303 :buffer-idx 363 :amp -4.5 :transp -5.281431 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15853894 :out1
   0 :out2 1)
  (:time 0.48637408 :buffer-idx 363 :amp -18.0 :transp -19.17907 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7745708 :out1 0
   :out2 1)
  (:time 0.48919457 :buffer-idx 361 :amp -18.0 :transp -8.319805 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36730075 :out1
   0 :out2 1)
  (:time 0.50568146 :buffer-idx 361 :amp -20.0 :transp -23.622849 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9909704 :out1
   0 :out2 1)
  (:time 0.5088962 :buffer-idx 362 :amp -12.5 :transp -21.345778 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.30957925 :out1
   0 :out2 1)
  (:time 0.51325136 :buffer-idx 363 :amp -25.5 :transp -16.354284 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7673547 :out1
   0 :out2 1)
  (:time 0.5259371 :buffer-idx 362 :amp -23.0 :transp -18.843998 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46907854 :out1
   0 :out2 1)
  (:time 0.53544503 :buffer-idx 361 :amp -10.0 :transp -13.4521885 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.050787687
   :out1 0 :out2 1)
  (:time 0.53726226 :buffer-idx 361 :amp -3.5 :transp -4.245885 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.30146194 :out1
   0 :out2 1)
  (:time 0.53815913 :buffer-idx 361 :amp -18.5 :transp -10.564592 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5412983 :out1
   0 :out2 1)
  (:time 0.54288673 :buffer-idx 363 :amp -20.0 :transp -23.398659 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47213423
   :out1 0 :out2 1)
  (:time 0.54361564 :buffer-idx 363 :amp -10.0 :transp -0.55700016 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0643965 :out1
   0 :out2 1)
  (:time 0.5438023 :buffer-idx 361 :amp -15.5 :transp -21.096296 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.1412983 :out1 0
   :out2 1)
  (:time 0.55571306 :buffer-idx 361 :amp -16.0 :transp -7.221097 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8897972 :out1 0
   :out2 1)
  (:time 0.55883926 :buffer-idx 362 :amp -16.5 :transp -15.897232 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.89919543
   :out1 0 :out2 1)
  (:time 0.5691561 :buffer-idx 363 :amp -16.0 :transp -18.477962 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.83115554 :out1
   0 :out2 1)
  (:time 0.5772825 :buffer-idx 363 :amp -27.0 :transp -23.191399 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6755775 :out1 0
   :out2 1)
  (:time 0.57738364 :buffer-idx 363 :amp -13.0 :transp -12.954998 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.667681 :out1
   0 :out2 1)
  (:time 0.58237547 :buffer-idx 362 :amp -23.5 :transp -20.820604 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.91937375
   :out1 0 :out2 1)
  (:time 0.5841508 :buffer-idx 361 :amp -19.5 :transp -9.920609 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10784495 :out1
   0 :out2 1)
  (:time 0.6009003 :buffer-idx 363 :amp -15.5 :transp -15.47554 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4042356 :out1 0
   :out2 1)
  (:time 0.60357046 :buffer-idx 361 :amp -11.5 :transp -2.9556513 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.48594165
   :out1 0 :out2 1)
  (:time 0.6069649 :buffer-idx 361 :amp -16.0 :transp -18.157745 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21997523 :out1
   0 :out2 1)
  (:time 0.6143104 :buffer-idx 361 :amp -8.0 :transp 1.0611544 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.19998407 :out1
   0 :out2 1)
  (:time 0.6149274 :buffer-idx 363 :amp -21.0 :transp -22.964561 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.95045805 :out1
   0 :out2 1)
  (:time 0.6201335 :buffer-idx 362 :amp -14.5 :transp -20.55074 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14601946 :out1
   0 :out2 1)
  (:time 0.62499285 :buffer-idx 363 :amp -10.0 :transp -12.390581 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.672246 :out1
   0 :out2 1)
  (:time 0.6255287 :buffer-idx 363 :amp -9.0 :transp -6.0679283 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2801758 :out1 0
   :out2 1)
  (:time 0.64784884 :buffer-idx 362 :amp -16.0 :transp -17.811487 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5051429 :out1
   0 :out2 1)
  (:time 0.65151715 :buffer-idx 362 :amp -14.5 :transp -14.968069 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.63943386
   :out1 0 :out2 1)
  (:time 0.651986 :buffer-idx 361 :amp -19.0 :transp -22.741255 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13094723 :out1
   0 :out2 1)
  (:time 0.6547378 :buffer-idx 361 :amp -21.5 :transp -8.932237 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.84290445 :out1
   0 :out2 1)
  (:time 0.6579232 :buffer-idx 362 :amp -18.5 :transp -20.280647 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.11055362 :out1
   0 :out2 1)
  (:time 0.6735475 :buffer-idx 361 :amp -19.0 :transp -11.814957 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.11460793 :out1
   0 :out2 1)
  (:time 0.67932975 :buffer-idx 363 :amp -15.0 :transp -5.1792765 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22121525
   :out1 0 :out2 1)
  (:time 0.6817552 :buffer-idx 363 :amp -3.5 :transp -1.4343233 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6681385 :out1 0
   :out2 1)
  (:time 0.68710244 :buffer-idx 363 :amp -20.0 :transp -17.479034 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.68381274
   :out1 0 :out2 1)
  (:time 0.6909841 :buffer-idx 362 :amp -23.0 :transp -22.506262 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5558249 :out1 0
   :out2 1)
  (:time 0.6959332 :buffer-idx 362 :amp -25.5 :transp -14.522767 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.18365216 :out1
   0 :out2 1)
  (:time 0.70012814 :buffer-idx 361 :amp -13.5 :transp -19.978998 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.21178329
   :out1 0 :out2 1)
  (:time 0.72982866 :buffer-idx 361 :amp -14.0 :transp -11.147732 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.36473942
   :out1 0 :out2 1)
  (:time 0.7302347 :buffer-idx 363 :amp -18.0 :transp -17.113735 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.52853227 :out1
   0 :out2 1)
  (:time 0.73451644 :buffer-idx 363 :amp -16.0 :transp -22.243946 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47762084
   :out1 0 :out2 1)
  (:time 0.73477715 :buffer-idx 362 :amp -20.5 :transp -7.81151 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6092874 :out1 0
   :out2 1)
  (:time 0.73949337 :buffer-idx 363 :amp -21.5 :transp -14.086044 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.536975 :out1
   0 :out2 1)
  (:time 0.7418122 :buffer-idx 362 :amp -23.5 :transp -19.681072 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.60101247 :out1
   0 :out2 1)
  (:time 0.7503155 :buffer-idx 361 :amp -12.0 :transp -4.0067816 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31789446 :out1
   0 :out2 1)
  (:time 0.7673881 :buffer-idx 362 :amp -5.0 :transp 4.5650024 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29655087 :out1
   0 :out2 1)
  (:time 0.77529216 :buffer-idx 361 :amp -17.0 :transp -21.998241 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26950395
   :out1 0 :out2 1)
  (:time 0.7806242 :buffer-idx 363 :amp -19.0 :transp -16.68697 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.41767085 :out1
   0 :out2 1)
  (:time 0.7860769 :buffer-idx 363 :amp -19.5 :transp -7.0932016 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22356558 :out1
   0 :out2 1)
  (:time 0.7880755 :buffer-idx 363 :amp -17.5 :transp -19.350416 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.63021696 :out1
   0 :out2 1)
  (:time 0.7977937 :buffer-idx 362 :amp -24.5 :transp -13.501541 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23095453 :out1
   0 :out2 1)
  (:time 0.8022396 :buffer-idx 362 :amp -14.0 :transp -10.289286 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.96416616 :out1
   0 :out2 1)
  (:time 0.81738144 :buffer-idx 363 :amp -24.0 :transp -21.744623 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6500412 :out1
   0 :out2 1)
  (:time 0.8210967 :buffer-idx 362 :amp -3.5 :transp 1.2770004 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.48196542 :out1
   0 :out2 1)
  (:time 0.826785 :buffer-idx 363 :amp -16.5 :transp -19.07375 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38741827 :out1
   0 :out2 1)
  (:time 0.8299171 :buffer-idx 361 :amp -22.0 :transp -16.269493 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.61590123 :out1
   0 :out2 1)
  (:time 0.8323143 :buffer-idx 361 :amp -8.0 :transp -2.652379 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9416094 :out1 0
   :out2 1)
  (:time 0.840493 :buffer-idx 361 :amp -12.5 :transp -6.331255 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8239846 :out1 0
   :out2 1)
  (:time 0.8467853 :buffer-idx 363 :amp -25.5 :transp -13.0103655 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8647591 :out1
   0 :out2 1)
  (:time 0.85699224 :buffer-idx 362 :amp -4.0 :transp 6.615982 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9778478 :out1 0
   :out2 1)
  (:time 0.85721815 :buffer-idx 363 :amp -16.0 :transp -9.637504 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.068472266 :out1
   0 :out2 1)
  (:time 0.8637488 :buffer-idx 362 :amp -20.0 :transp -21.465225 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.90486443 :out1
   0 :out2 1)
  (:time 0.87332565 :buffer-idx 361 :amp -18.0 :transp -15.90185 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.34974456 :out1
   0 :out2 1)
  (:time 0.8753602 :buffer-idx 362 :amp -16.5 :transp -18.726572 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8582394 :out1 0
   :out2 1)
  (:time 0.8954435 :buffer-idx 362 :amp -18.5 :transp -12.5225315 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.37534058
   :out1 0 :out2 1)
  (:time 0.9090989 :buffer-idx 361 :amp -16.0 :transp -21.191956 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44568586 :out1
   0 :out2 1)
  (:time 0.915896 :buffer-idx 363 :amp -6.5 :transp 3.1216154 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.72559774 :out1
   0 :out2 1)
  (:time 0.9206734 :buffer-idx 363 :amp -15.5 :transp -18.402708 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6213248 :out1 0
   :out2 1)
  (:time 0.9253106 :buffer-idx 361 :amp -14.0 :transp -8.830254 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5225799 :out1 0
   :out2 1)
  (:time 0.9274684 :buffer-idx 362 :amp -19.0 :transp -15.443297 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7385113 :out1 0
   :out2 1)
  (:time 0.9296505 :buffer-idx 361 :amp -13.0 :transp -1.0446444 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.67400193 :out1
   0 :out2 1)
  (:time 0.9303824 :buffer-idx 363 :amp -14.5 :transp -5.072607 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6537585 :out1 0
   :out2 1)
  (:time 0.9518875 :buffer-idx 362 :amp -18.0 :transp -20.934124 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16734755 :out1
   0 :out2 1)
  (:time 0.969861 :buffer-idx 361 :amp -21.5 :transp -11.776443 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5520048 :out1 0
   :out2 1)
  (:time 0.97003067 :buffer-idx 363 :amp -13.5 :transp -18.04994 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.09651077 :out1
   0 :out2 1)
  (:time 0.9907263 :buffer-idx 363 :amp -17.0 :transp -14.907546 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8460432 :out1 0
   :out2 1)
  (:time 1.0004182 :buffer-idx 362 :amp -18.0 :transp -20.64169 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8913841 :out1 0
   :out2 1)
  (:time 1.012528 :buffer-idx 362 :amp -19.5 :transp -3.922389 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9104295 :out1 0
   :out2 1)
  (:time 1.0142893 :buffer-idx 361 :amp -14.0 :transp -7.7753935 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47872198 :out1
   0 :out2 1)
  (:time 1.0163944 :buffer-idx 362 :amp -24.5 :transp -11.309912 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4820907 :out1 0
   :out2 1)
  (:time 1.0167849 :buffer-idx 362 :amp -14.0 :transp 10.273529 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47862625 :out1
   0 :out2 1)
  (:time 1.0183624 :buffer-idx 362 :amp -19.0 :transp 0.42064095 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.64556706 :out1
   0 :out2 1)
  (:time 1.0236542 :buffer-idx 362 :amp -14.5 :transp -17.66668 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.57109976 :out1
   0 :out2 1)
  (:time 1.0455482 :buffer-idx 361 :amp -23.0 :transp -20.369747 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.54384243 :out1
   0 :out2 1)
  (:time 1.0504723 :buffer-idx 363 :amp -17.0 :transp -14.401539 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.77678657 :out1
   0 :out2 1)
  (:time 1.0793549 :buffer-idx 362 :amp -22.5 :transp -17.268574 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7204802 :out1 0
   :out2 1)
  (:time 1.0908676 :buffer-idx 362 :amp -18.5 :transp -10.563264 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9432868 :out1 0
   :out2 1)
  (:time 1.0912769 :buffer-idx 363 :amp -22.0 :transp -20.094198 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43223882 :out1
   0 :out2 1)
  (:time 1.0978143 :buffer-idx 362 :amp -16.0 :transp 1.7329769 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.49844754 :out1
   0 :out2 1)
  (:time 1.0996963 :buffer-idx 363 :amp -14.5 :transp -2.7018414 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.50937283 :out1
   0 :out2 1)
  (:time 1.1027924 :buffer-idx 363 :amp -17.0 :transp -13.958423 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 4.4977665e-4
   :out1 0 :out2 1)
  (:time 1.1074889 :buffer-idx 362 :amp -13.0 :transp -6.670492 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.057399154 :out1
   0 :out2 1)
  (:time 1.1264921 :buffer-idx 363 :amp -5.5 :transp 7.2194204 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.970834 :out1 0
   :out2 1)
  (:time 1.1315311 :buffer-idx 362 :amp -27.0 :transp -19.851635 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7078403 :out1 0
   :out2 1)
  (:time 1.1322188 :buffer-idx 363 :amp -14.5 :transp -16.890743 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7577071 :out1 0
   :out2 1)
  (:time 1.1583048 :buffer-idx 363 :amp -21.0 :transp -13.48827 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.053984642 :out1
   0 :out2 1)
  (:time 1.1782386 :buffer-idx 361 :amp -25.0 :transp -19.570189 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17669058 :out1
   0 :out2 1)
  (:time 1.1812938 :buffer-idx 361 :amp -19.5 :transp -9.656675 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22338402 :out1
   0 :out2 1)
  (:time 1.1882036 :buffer-idx 361 :amp -15.0 :transp -5.713604 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2432568 :out1 0
   :out2 1)
  (:time 1.1919312 :buffer-idx 362 :amp -21.5 :transp -16.463963 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.03316474 :out1
   0 :out2 1)
  (:time 1.1921213 :buffer-idx 362 :amp -7.0 :transp 14.286861 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.65042126 :out1
   0 :out2 1)
  (:time 1.200772 :buffer-idx 361 :amp -22.5 :transp -1.28656 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17368352 :out1
   0 :out2 1)
  (:time 1.2035477 :buffer-idx 361 :amp -8.0 :transp 3.479412 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.017001629 :out1
   0 :out2 1)
  (:time 1.2049452 :buffer-idx 363 :amp -16.0 :transp -13.093256 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5934901 :out1 0
   :out2 1)
  (:time 1.2223998 :buffer-idx 361 :amp -21.0 :transp -19.304085 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07749951 :out1
   0 :out2 1)
  (:time 1.2593956 :buffer-idx 363 :amp -18.5 :transp -15.981779 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.70753217 :out1
   0 :out2 1)
  (:time 1.2701933 :buffer-idx 362 :amp -19.0 :transp -19.01609 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.25260723 :out1
   0 :out2 1)
  (:time 1.2712448 :buffer-idx 362 :amp -21.0 :transp -12.531744 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.52627146 :out1
   0 :out2 1)
  (:time 1.2789712 :buffer-idx 363 :amp -24.5 :transp -8.67739 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8988764 :out1 0
   :out2 1)
  (:time 1.298465 :buffer-idx 362 :amp -14.5 :transp 0.081357956 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6677053 :out1 0
   :out2 1)
  (:time 1.3037406 :buffer-idx 362 :amp -16.5 :transp -15.664835 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26472676 :out1
   0 :out2 1)
  (:time 1.3105503 :buffer-idx 363 :amp -14.0 :transp -4.263158 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26173878 :out1
   0 :out2 1)
  (:time 1.3249656 :buffer-idx 361 :amp -24.0 :transp -18.686049 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6498705 :out1 0
   :out2 1)
  (:time 1.3298085 :buffer-idx 361 :amp -25.0 :transp -12.035749 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.989382 :out1 0
   :out2 1)
  (:time 1.3314182 :buffer-idx 363 :amp -18.0 :transp 5.5914917 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8894603 :out1 0
   :out2 1)
  (:time 1.3423182 :buffer-idx 362 :amp -23.5 :transp -8.042291 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6400411 :out1 0
   :out2 1)
  (:time 1.3490881 :buffer-idx 361 :amp -17.5 :transp -15.340726 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.69576275 :out1
   0 :out2 1)
  (:time 1.3524618 :buffer-idx 363 :amp -12.5 :transp 11.616366 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7200389 :out1 0
   :out2 1)
  (:time 1.3709011 :buffer-idx 362 :amp -22.0 :transp -18.409252 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.56395626 :out1
   0 :out2 1)
  (:time 1.3766658 :buffer-idx 363 :amp -5.0 :transp 18.510962 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51615465 :out1
   0 :out2 1)
  (:time 1.3918586 :buffer-idx 361 :amp -18.0 :transp -11.510226 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7829524 :out1 0
   :out2 1)
  (:time 1.3919547 :buffer-idx 362 :amp -19.5 :transp 1.3904152 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16473389 :out1
   0 :out2 1)
  (:time 1.4035784 :buffer-idx 363 :amp -20.5 :transp -7.428112 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6804489 :out1 0
   :out2 1)
  (:time 1.4160794 :buffer-idx 361 :amp -19.0 :transp -18.13702 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7256285 :out1 0
   :out2 1)
  (:time 1.419614 :buffer-idx 361 :amp -18.5 :transp -14.836661 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.34445655 :out1
   0 :out2 1)
  (:time 1.4653337 :buffer-idx 363 :amp -11.0 :transp -2.4281693 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6254226 :out1 0
   :out2 1)
  (:time 1.467485 :buffer-idx 361 :amp -14.5 :transp -14.494515 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4565693 :out1 0
   :out2 1)
  (:time 1.4699811 :buffer-idx 363 :amp -9.0 :transp 7.880184 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.08952916 :out1
   0 :out2 1)
  (:time 1.476734 :buffer-idx 362 :amp -21.0 :transp -10.7913885 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.79165435 :out1
   0 :out2 1)
  (:time 1.4813895 :buffer-idx 362 :amp -23.0 :transp -17.743477 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42861938 :out1
   0 :out2 1)
  (:time 1.5058738 :buffer-idx 363 :amp -16.5 :transp -6.402528 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42523098 :out1
   0 :out2 1)
  (:time 1.5197759 :buffer-idx 363 :amp -21.5 :transp -14.12078 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45140743 :out1
   0 :out2 1)
  (:time 1.533885 :buffer-idx 363 :amp -10.5 :transp 15.146522 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8472265 :out1 0
   :out2 1)
  (:time 1.5445368 :buffer-idx 361 :amp -22.0 :transp -17.362967 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.51848006 :out1
   0 :out2 1)
  (:time 1.5518134 :buffer-idx 362 :amp -17.0 :transp -10.155518 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.67609715 :out1
   0 :out2 1)
  (:time 1.5619384 :buffer-idx 363 :amp -16.5 :transp 3.7705612 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.311491 :out1 0
   :out2 1)
  (:time 1.5676184 :buffer-idx 363 :amp -22.5 :transp -13.778838 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43876886 :out1
   0 :out2 1)
  (:time 1.5877455 :buffer-idx 363 :amp -9.0 :transp 23.342438 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9678047 :out1 0
   :out2 1)
  (:time 1.5918058 :buffer-idx 362 :amp -23.5 :transp -5.5409966 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6440636 :out1 0
   :out2 1)
  (:time 1.6133491 :buffer-idx 361 :amp -22.0 :transp -16.948322 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22397304 :out1
   0 :out2 1)
  (:time 1.6222527 :buffer-idx 361 :amp -21.5 :transp -13.388353 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46250117 :out1
   0 :out2 1)
  (:time 1.6334028 :buffer-idx 362 :amp -12.0 :transp -0.43567657 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2769705 :out1
   0 :out2 1)
  (:time 1.642361 :buffer-idx 363 :amp -16.0 :transp -9.388639 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29530656 :out1
   0 :out2 1)
  (:time 1.655176 :buffer-idx 363 :amp -18.5 :transp 5.076092 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23392427 :out1
   0 :out2 1)
  (:time 1.6594325 :buffer-idx 361 :amp -22.5 :transp -4.8629894 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22387195 :out1
   0 :out2 1)
  (:time 1.6710871 :buffer-idx 362 :amp -21.5 :transp -13.039323 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38773835 :out1
   0 :out2 1)
  (:time 1.6841716 :buffer-idx 363 :amp -24.0 :transp -16.52156 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17116392 :out1
   0 :out2 1)
  (:time 1.7228096 :buffer-idx 363 :amp -23.0 :transp -8.707295 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.044268608 :out1
   0 :out2 1)
  (:time 1.7256656 :buffer-idx 363 :amp -14.5 :transp -12.649237 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5861262 :out1 0
   :out2 1)
  (:time 1.7310319 :buffer-idx 361 :amp -14.5 :transp -4.145155 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6809778 :out1 0
   :out2 1)
  (:time 1.7422245 :buffer-idx 361 :amp -19.0 :transp -16.17175 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3505931 :out1 0
   :out2 1)
  (:time 1.7504346 :buffer-idx 362 :amp -14.0 :transp 0.95175934 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8765743 :out1 0
   :out2 1)
  (:time 1.7939738 :buffer-idx 361 :amp -6.5 :transp 20.207363 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6114235 :out1 0
   :out2 1)
  (:time 1.8101959 :buffer-idx 361 :amp -22.5 :transp -3.3514786 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.11541629 :out1
   0 :out2 1)
  (:time 1.8133997 :buffer-idx 362 :amp -24.0 :transp -15.742866 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46798134 :out1
   0 :out2 1)
  (:time 1.815182 :buffer-idx 363 :amp -20.5 :transp -12.009442 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.71256495 :out1
   0 :out2 1)
  (:time 1.8351927 :buffer-idx 363 :amp -18.0 :transp -7.7554846 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.26640642 :out1
   0 :out2 1)
  (:time 1.8365177 :buffer-idx 362 :amp -12.0 :transp 13.934395 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7960082 :out1 0
   :out2 1)
  (:time 1.853276 :buffer-idx 362 :amp -17.0 :transp 2.1709652 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.83378816 :out1
   0 :out2 1)
  (:time 1.8744886 :buffer-idx 362 :amp -21.5 :transp -11.585564 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.13032043 :out1
   0 :out2 1)
  (:time 1.8783125 :buffer-idx 362 :amp -21.5 :transp -2.668562 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0021795034
   :out1 0 :out2 1)
  (:time 1.8901849 :buffer-idx 362 :amp -19.0 :transp -15.280178 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.35731828 :out1
   0 :out2 1)
  (:time 1.952463 :buffer-idx 361 :amp -25.0 :transp -6.7622833 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3370595 :out1 0
   :out2 1)
  (:time 1.9625052 :buffer-idx 361 :amp -26.0 :transp -14.844394 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6074778 :out1 0
   :out2 1)
  (:time 1.964133 :buffer-idx 363 :amp -16.5 :transp -10.944853 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9432448 :out1 0
   :out2 1)
  (:time 1.9700222 :buffer-idx 362 :amp -19.5 :transp 9.484629 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.021425724 :out1
   0 :out2 1)
  (:time 1.9932667 :buffer-idx 362 :amp -9.0 :transp 3.8305836 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.17338622 :out1
   0 :out2 1)
  (:time 2.0166519 :buffer-idx 362 :amp -23.0 :transp -14.51812 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2526921 :out1 0
   :out2 1)
  (:time 2.0371764 :buffer-idx 361 :amp -15.5 :transp -1.0758343 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.61635077 :out1
   0 :out2 1)
  (:time 2.038057 :buffer-idx 361 :amp -18.5 :transp -10.416499 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.10133934 :out1
   0 :out2 1)
  (:time 2.0516658 :buffer-idx 361 :amp -15.0 :transp -5.922104 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.98306537 :out1
   0 :out2 1)
  (:time 2.1042821 :buffer-idx 362 :amp -21.0 :transp -13.990082 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4020108 :out1 0
   :out2 1)
  (:time 2.108133 :buffer-idx 362 :amp -21.5 :transp -9.91565 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.325822 :out1 0
   :out2 1)
  (:time 2.1087186 :buffer-idx 361 :amp -17.5 :transp 11.4266815 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5952203 :out1 0
   :out2 1)
  (:time 2.1089537 :buffer-idx 363 :amp -10.0 :transp 5.202078 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5434879 :out1 0
   :out2 1)
  (:time 2.1239765 :buffer-idx 362 :amp -20.5 :transp -0.20560265 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47346497
   :out1 0 :out2 1)
  (:time 2.1333654 :buffer-idx 363 :amp -15.0 :transp -5.2301636 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.565091 :out1 0
   :out2 1)
  (:time 2.1599038 :buffer-idx 363 :amp -24.0 :transp -13.65492 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.63088644 :out1
   0 :out2 1)
  (:time 2.2001553 :buffer-idx 361 :amp -22.5 :transp -9.257945 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8306105 :out1 0
   :out2 1)
  (:time 2.21721 :buffer-idx 362 :amp -4.5 :transp 28.442741 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6368375 :out1 0
   :out2 1)
  (:time 2.2378104 :buffer-idx 363 :amp -12.0 :transp 6.729698 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.817186 :out1 0
   :out2 1)
  (:time 2.2438707 :buffer-idx 363 :amp -25.0 :transp -13.148954 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2880553 :out1 0
   :out2 1)
  (:time 2.2603066 :buffer-idx 363 :amp -16.5 :transp 1.1612053 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9852338 :out1 0
   :out2 1)
  (:time 2.2665083 :buffer-idx 361 :amp -17.5 :transp -8.783705 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.57006586 :out1
   0 :out2 1)
  (:time 2.303552 :buffer-idx 361 :amp -26.0 :transp -3.7887993 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.501596 :out1 0
   :out2 1)
  (:time 2.323179 :buffer-idx 363 :amp -22.0 :transp -12.671063 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.89400446 :out1
   0 :out2 1)
  (:time 2.3604596 :buffer-idx 362 :amp -16.5 :transp 2.1653118 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.057899952 :out1
   0 :out2 1)
  (:time 2.3604608 :buffer-idx 362 :amp -20.5 :transp -8.1122055 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.96417487 :out1
   0 :out2 1)
  (:time 2.3775473 :buffer-idx 361 :amp -18.0 :transp -3.1621094 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07356143 :out1
   0 :out2 1)
  (:time 2.3991141 :buffer-idx 361 :amp -26.0 :transp -12.213498 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8736819 :out1 0
   :out2 1)
  (:time 2.406623 :buffer-idx 362 :amp -19.5 :transp 15.597994 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31080997 :out1
   0 :out2 1)
  (:time 2.4792497 :buffer-idx 362 :amp -25.0 :transp -11.730621 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.46773505 :out1
   0 :out2 1)
  (:time 2.4979012 :buffer-idx 363 :amp -15.5 :transp -7.1298847 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24549639 :out1
   0 :out2 1)
  (:time 2.5022006 :buffer-idx 363 :amp -25.5 :transp 3.5863705 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9890677 :out1 0
   :out2 1)
  (:time 2.534371 :buffer-idx 363 :amp -21.0 :transp -11.398475 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.89904237 :out1
   0 :out2 1)
  (:time 2.5592444 :buffer-idx 361 :amp -12.0 :transp 10.540365 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4855578 :out1 0
   :out2 1)
  (:time 2.5799491 :buffer-idx 362 :amp -23.0 :transp -1.4479027 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.61497223 :out1
   0 :out2 1)
  (:time 2.5840514 :buffer-idx 363 :amp -10.0 :transp 26.281664 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3739797 :out1 0
   :out2 1)
  (:time 2.6061018 :buffer-idx 363 :amp -17.5 :transp 18.391134 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24199283 :out1
   0 :out2 1)
  (:time 2.6142676 :buffer-idx 363 :amp -20.5 :transp -6.2981853 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.43942642 :out1
   0 :out2 1)
  (:time 2.6460226 :buffer-idx 361 :amp -19.0 :transp -10.72569 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8183011 :out1 0
   :out2 1)
  (:time 2.6868975 :buffer-idx 363 :amp -25.0 :transp -0.54212 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.894788 :out1 0
   :out2 1)
  (:time 2.6945214 :buffer-idx 361 :amp -22.5 :transp -5.724592 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5298853 :out1 0
   :out2 1)
  (:time 2.7208765 :buffer-idx 362 :amp -18.0 :transp -10.274637 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7852231 :out1 0
   :out2 1)
  (:time 2.7732437 :buffer-idx 361 :amp -24.5 :transp 6.303772 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.38385952 :out1
   0 :out2 1)
  (:time 2.8038824 :buffer-idx 362 :amp -15.5 :transp -4.9429646 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16522694 :out1
   0 :out2 1)
  (:time 2.838075 :buffer-idx 363 :amp -9.0 :transp 13.845959 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.94943655 :out1
   0 :out2 1)
  (:time 2.8574824 :buffer-idx 361 :amp -22.0 :transp -9.451485 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.44442034 :out1
   0 :out2 1)
  (:time 2.9547205 :buffer-idx 363 :amp -15.0 :transp 1.72616 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.29475737 :out1
   0 :out2 1)
  (:time 2.9900842 :buffer-idx 363 :amp -20.5 :transp 8.477753 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.07536042 :out1
   0 :out2 1)
  (:time 2.9979947 :buffer-idx 363 :amp -24.0 :transp -8.604794 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.54246557 :out1
   0 :out2 1)
  (:time 3.0041666 :buffer-idx 362 :amp -22.5 :transp -3.511486 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7985953 :out1 0
   :out2 1)
  (:time 3.1458502 :buffer-idx 363 :amp -23.0 :transp -7.713852 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.78051317 :out1
   0 :out2 1)
  (:time 3.2045388 :buffer-idx 363 :amp -22.5 :transp -2.0793762 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.760833 :out1 0
   :out2 1)
  (:time 3.220321 :buffer-idx 362 :amp -19.0 :transp 3.9756165 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.23979092 :out1
   0 :out2 1)
  (:time 3.2323995 :buffer-idx 363 :amp -24.0 :transp -7.1923294 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.12972784 :out1
   0 :out2 1)
  (:time 3.3455837 :buffer-idx 362 :amp -16.5 :transp -1.0712967 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16942382 :out1
   0 :out2 1)
  (:time 3.3544128 :buffer-idx 361 :amp -20.0 :transp 5.1112823 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.22563493 :out1
   0 :out2 1)
  (:time 3.411476 :buffer-idx 361 :amp -22.0 :transp -6.1132584 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.4414326 :out1 0
   :out2 1)
  (:time 3.423589 :buffer-idx 363 :amp -24.5 :transp 12.823952 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7308574 :out1 0
   :out2 1)
  (:time 3.4429638 :buffer-idx 363 :amp -12.5 :transp -0.37529755 :start 0 :end
   0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.16222715
   :out1 0 :out2 1)
  (:time 3.484974 :buffer-idx 361 :amp -19.0 :transp -5.6703777 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.40113473 :out1
   0 :out2 1)
  (:time 3.5152597 :buffer-idx 363 :amp -15.0 :transp 6.473549 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.42207968 :out1
   0 :out2 1)
  (:time 3.5674622 :buffer-idx 362 :amp -19.0 :transp -5.1733246 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5031898 :out1 0
   :out2 1)
  (:time 3.658299 :buffer-idx 363 :amp -22.5 :transp 1.1637554 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6233957 :out1 0
   :out2 1)
  (:time 3.7221258 :buffer-idx 362 :amp -18.0 :transp -4.2413616 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.03498745 :out1
   0 :out2 1)
  (:time 3.7348106 :buffer-idx 363 :amp -18.0 :transp 8.332996 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6923733 :out1 0
   :out2 1)
  (:time 3.7653117 :buffer-idx 362 :amp -13.5 :transp 1.9286022 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.15409195 :out1
   0 :out2 1)
  (:time 3.7894354 :buffer-idx 362 :amp -19.0 :transp 25.12453 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.0812614 :out1 0
   :out2 1)
  (:time 3.8490145 :buffer-idx 361 :amp -25.0 :transp -3.476759 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.713287 :out1 0
   :out2 1)
  (:time 3.8509567 :buffer-idx 362 :amp -14.5 :transp 17.10862 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.58672416 :out1
   0 :out2 1)
  (:time 3.9367993 :buffer-idx 363 :amp -19.0 :transp -2.947792 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.2783097 :out1 0
   :out2 1)
  (:time 3.9818418 :buffer-idx 363 :amp -20.5 :transp 3.4761925 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.717361 :out1 0
   :out2 1)
  (:time 4.0199833 :buffer-idx 361 :amp -26.0 :transp -2.4465466 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.6997875 :out1 0
   :out2 1)
  (:time 4.0451474 :buffer-idx 361 :amp -24.0 :transp 10.961338 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.28616703 :out1
   0 :out2 1)
  (:time 4.114053 :buffer-idx 363 :amp -19.5 :transp 19.74635 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3493073 :out1 0
   :out2 1)
  (:time 4.2584543 :buffer-idx 363 :amp -19.0 :transp -1.0095787 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.19842398 :out1
   0 :out2 1)
  (:time 4.3586154 :buffer-idx 363 :amp -18.5 :transp 22.198265 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.5394844 :out1 0
   :out2 1)
  (:time 4.424343 :buffer-idx 363 :amp -19.5 :transp 6.638857 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.016201735 :out1
   0 :out2 1)
  (:time 4.505687 :buffer-idx 361 :amp -25.0 :transp 0.48018646 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.31135917 :out1
   0 :out2 1)
  (:time 4.5076838 :buffer-idx 363 :amp -22.0 :transp 14.878712 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.3602084 :out1 0
   :out2 1)
  (:time 4.596211 :buffer-idx 361 :amp -20.0 :transp 1.0256596 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.14908421 :out1
   0 :out2 1)
  (:time 4.624225 :buffer-idx 362 :amp -14.5 :transp 8.067459 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59281766 :out1
   0 :out2 1)
  (:time 4.667653 :buffer-idx 361 :amp -20.0 :transp 16.233543 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.97154737 :out1
   0 :out2 1)
  (:time 4.742827 :buffer-idx 361 :amp -22.0 :transp 1.9091301 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.7565906 :out1 0
   :out2 1)
  (:time 4.9702263 :buffer-idx 361 :amp -14.5 :transp 28.330109 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9042958 :out1 0
   :out2 1)
  (:time 4.9857473 :buffer-idx 361 :amp -19.0 :transp 3.3729076 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9176824 :out1 0
   :out2 1)
  (:time 5.2621202 :buffer-idx 363 :amp -20.0 :transp 5.0382633 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.74022114 :out1
   0 :out2 1)
  (:time 5.318027 :buffer-idx 361 :amp -14.5 :transp 13.02623 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.59317195 :out1
   0 :out2 1)
  (:time 5.798589 :buffer-idx 363 :amp -24.0 :transp 8.270887 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.45756042 :out1
   0 :out2 1)
  (:time 5.969603 :buffer-idx 362 :amp -23.0 :transp 27.260181 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.94068706 :out1
   0 :out2 1)
  (:time 6.0131865 :buffer-idx 363 :amp -17.5 :transp 17.994701 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9275601 :out1 0
   :out2 1)
  (:time 6.445077 :buffer-idx 363 :amp -18.5 :transp 21.081524 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.24151444 :out1
   0 :out2 1)
  (:time 6.4693155 :buffer-idx 362 :amp -24.0 :transp 12.312517 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9453261 :out1 0
   :out2 1)
  (:time 6.7112293 :buffer-idx 362 :amp -20.0 :transp 13.770227 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.20123804 :out1
   0 :out2 1)
  (:time 6.9992146 :buffer-idx 361 :amp -23.0 :transp 15.50556 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.9218391 :out1 0
   :out2 1)
  (:time 7.380302 :buffer-idx 362 :amp -16.0 :transp 17.801897 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.8060899 :out1 0
   :out2 1)
  (:time 7.9648685 :buffer-idx 362 :amp -17.0 :transp 21.324347 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.47980368 :out1
   0 :out2 1)
  (:time 8.757102 :buffer-idx 363 :amp -20.0 :transp 26.098146 :start 0 :end 0
   :stretch 1.0 :wwidth 123 :attack 0 :release 0.01 :pan 0.053688765 :out1
   0 :out2 1))
