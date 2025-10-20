;;; 
;;; cl-poolplayer.lisp
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

(in-package :cl-poolplayer)

(setf *print-case* :downcase)

(defparameter *poolplayer-events* nil
  "global storage for recording")

(defparameter *poolplayer-recording-p* nil
  "flag to indicate recording of events.")

(defun normalize-x (curr-time end-time dur)
  "given curr-time, end-time and total-duration of a preset, return
the curr-time as a normalized value in relation to the position
between start and end-time."
  (if (or (not dur) (zerop dur))
      0
      (let ((start-time (- end-time dur)))
        (float (/ (- curr-time start-time) dur) 1.0))))

(defun distributed-play (params)
  "play on local and remote machines."
  (if *debug* (format t "~&~a" params))
  (apply #'play-buffer-stretch-env-pan-out* params)
;;;  (apply #'send-to-remote params)
  )

;;; (defgeneric perform (player time args))

;;; perform routine:

(defun perform (player time args)
  "central (tail call) recursive perform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the calculated time for the next event
is before the end time of the player's life cycle or end is
nil. Otherwise it just sets the 'playing slot of the player to nil and
returns."
  (with-slots (playing preset-no id start end idx dur) player
    (let* ((effective-preset-no (if (= -1 preset-no) *curr-poolplayer-preset-no* preset-no))
           (x (normalize-x time end dur))
           (prst (aref *poolplayer-presets* effective-preset-no)))
      (unless (dtime-fn prst) (error "dtime-fn of preset ~a undefined. Did you forget to digest the preset?" effective-preset-no))
      (unless (params-fn prst) (error "params-fn of preset ~a undefined. Did you forget to digest the preset?" effective-preset-no))
      (when playing
        (let* ((dtime (apply (dtime-fn prst) x dur args))
               (next (+ time dtime))
               (params (apply (params-fn prst) x dtime dur idx args))
	       (playfn (getf params :play)))
          (incf idx)
          (incf (getf params :amp) *master-amp-db*)
          (setf params (list* :buffer (lsample-buffer (getf params :lsample)) params))
          (incudine.util:msg :info "~S" params)
          (dolist (key '(:adjust-stretch :dy)) (remf params key))
          (when *poolplayer-recording-p* (push (cons time (copy-seq params)) *poolplayer-events*))
          (dolist (key '(:lsample :keynum :adjust-stretch :play)) (remf params key))
          (incudine.util:msg :info "~S" params)
          (apply playfn :head 200 params)
          (if (and dur (> next end))
              (setf playing nil)
              (at next #'perform player next args)))))))

(defun stop (p)
  (sv p :playing nil))

(defun calc-inits (inits)
  (loop
    for (key val) on inits by #'cddr
    append (list key (eval val))))

(defun preset-play (preset-no dur &rest args)
  (let* ((time (getf args :at (now)))
	 (player (or (getf args :player) (make-eventplayer)))
	 (preset-form (get-preset-form preset-no))
	 (dur (or dur (eval (getf preset-form :dur)))))
    (cm::sv player
      :start-time time
      :end (if dur (+ time dur))
      :dur dur
      :preset-no preset-no)
    (typecase (playing player)
      (cl-refs:ref-object nil)
      (otherwise
       (cm:sv player :playing t))
      )
    (setf (getf args :player) player)
    (let* ((inits (calc-inits (getf preset-form :inits))))
      (funcall #'perform player time (append inits args)))
    nil))
