;;; 
;;; cm-poolplayer.lisp
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


(in-package :cm)

(defobject poolevt (event)
    ((buffer-idx :initform 0 :accessor poolevt-buffer-idx)
     (amp :initform -9.0 :accessor poolevt-amp)
     (transp :initform -13.0 :accessor poolevt-transp)
     (start :initform 0 :accessor poolevt-start)
     (end :initform 0 :accessor poolevt-end)
     (stretch :initform 1.0 :accessor poolevt-stretch)
     (wwidth :initform 123 :accessor poolevt-wwidth)
     (attack :initform 0 :accessor poolevt-attack)
     (release :initform 0.01 :accessor poolevt-release)
     (pan :initform 0.5 :accessor poolevt-pan)
     (out1 :initform 0 :accessor poolevt-out1)
     (out2 :initform 1 :accessor poolevt-out2))
  (:parameters time buffer-idx amp transp start end stretch wwidth attack release pan out1 out2)
  (:event-streams))

(defmethod write-event ((obj poolevt) (fil svg-file) scoretime)
  "convert a midi object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (let* ((myid (poolevt-buffer-idx obj))
         (x-scale (x-scale fil))
         (stroke-width 0.5)
         (line (let ((x1 (* x-scale scoretime))
                     (y1 (* 1 (poolevt-transp obj)))
                     (width (* x-scale (* (poolevt-stretch obj) (- (poolevt-end obj) (poolevt-start obj)))))
                     (color (chan->color myid))
                     (opacity (max 0.0 (min (ou:db->amp (poolevt-amp obj)) 1.0))))
                 (make-instance 'svg-ie::svg-line :x1 (float x1) :y1 (float y1)
                                :x2 (float (+ x1 width)) :y2 (float y1)
                                :stroke-width stroke-width
                                :opacity opacity
                                :stroke-color color 
                                ;; :fill-color color
                                :id (new-id fil 'line-ids)))))
    (svg-file-insert-line line myid fil)))


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

(export '(poolevt poolevt-buffer-idx poolevt-amp poolevt-start poolevt-end poolevt-stretch poolevt-wwidth poolevt-attack poolevt-release poolevt-pan poolevt-out1 poolevt-out2) 'cm)
