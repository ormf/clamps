;;; 
;;; init.lisp
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

(cd "/home/orm/work/kompositionen/big-orchestra/lisp/big-orchestra")
(setf *print-case* :downcase)

(rts :rt-wait 5)

;;; (setf *buffers* (load-sounds "../../snd/*.wav"))



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
  (load-bo-sounds "../../snd/bo-samples")

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

(setf *players* (make-array 16
                            :element-type 'eventplayer
                            :initial-contents
                            (loop
                              for x below 16
                              collect (make-instance 'eventplayer))))

(defun start-bo-seqs (time) 
  (start-seq #'main-seq-01)
  (at (+ time (+ 30 (random 30))) #'start-seq #'main-seq-02)
  (at (+ time (+ 10 (random 20))) #'start-seq #'main-seq-03))

;;; (start-seq #'main-seq-02)

(defun init-bo (time)
  time
  (load-presets)
  (init-netsend)
  (init-midi)
  (def-sequences)
  (stop-all-seqs)
  ;;  (audio-test 13)
  )
;;; (incudine:rt-start)
(init-bo 5)
;; (start-bo-seqs (+ (now) 10))

#|

(show-playing-seqs)
(setf *emcs-conn* swank::*emacs-connection*)

(defun delete-other-windows
    (let ((swank::*emacs-connection* *emcs-conn*))
      (swank::eval-in-emacs '(delete-other-windows) t)))
|#
;;; (at (+ (now) 10) #'delete-other-windows))


#|
(show-playing-seqs)
|#
;;; (at (+ (now  0)) #'start-bo-seqs (+ (now) 0))

;;; (at (+ (now) 5) #'init-bo (+ (now) 5))

;;; (funcall (song-playfn e-guitar-01) 20)

;;; (show-playing-seqs)
;;; (setf *show-song* t)
;;; (setf *show-song* nil)

;;; (stop-all-seqs)
#|
(make-seq-player #'main-seq-01)

*remote-ip*

|#
