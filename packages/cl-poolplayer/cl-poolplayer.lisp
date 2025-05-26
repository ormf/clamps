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
  "global storage for eventplotter")

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

(defgeneric perform (player time args))

;;; perform routine:

(defmethod perform ((player eventplayer) time args)
  "central (tail call) recursive perform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the calculated time for the next event
is before the end time of the player's life cycle or end is
nil. Otherwise it just sets the 'playing slot of the player to nil and
returns."
  (with-slots (playing preset id start end dur) player
    (let* ((x (normalize-x time end dur))
           (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (apply (params-fn prst) x dur args)))
;;;        (break "preset-no: ~a, prst: ~a" preset prst)
      (when playing
        (let* ((next (+ time (apply (dtime-fn prst) x dur args))))
          (incf (getf params :amp) *master-amp-db*)
          (incudine.util:msg :info "~S" params)
          (dolist (key '(:lsample :keynum :dy :snd-id :adjust-stretch)) (remf params key))
;;;            (incudine.util:msg :info "~S" params)
          (apply #'of-incudine-dsps::play-buffer-stretch-env-pan-out* :head 200 params)
          (if (and dur (> next end))
              (setf playing nil)
              (at next #'perform player next args)))))))

(defmethod perform ((player eventplotter) time args)
  "central perform routine used by #'preset-play: It calculates params
according to the preset definition used by the player and pushes them
as property list with prepended time to result. It then reschedules
itself in case the calculated time for the next event is before the
end time of the player's life cycle. Otherwise it just sets the
'playing slot of the player to nil and returns the accumulated
result."
  (let ((result '()))
    (labels ((inner (player time args)
               (with-slots (playing preset id start end dur) player
                 (let* ((x (normalize-x time end dur))
                        (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
                        (params (apply (params-fn prst) x dur args)))
;;;        (break "preset-no: ~a, prst: ~a" preset prst)
                   (when playing
                     (let* ((next (+ time (apply (dtime-fn prst) x dur args))))
                       (incf (getf params :amp) *master-amp-db*)
                       (incudine.util:msg :info "~S" params)
                       (dolist (key '(:lsample :keynum :dy :snd-id :adjust-stretch)) (remf params key))
;;;            (incudine.util:msg :info "~S" params)
                       (push (cons time params) result)
                       (if (and dur (> next end))
                           (setf playing nil)
                           (inner player next args))))))))
      (inner player time args)
      (reverse result))))

(defgeneric stop (p))

(defmethod stop ((p eventplayer))
  (sv p :playing nil))

(defgeneric preset-play (player preset dur &rest args))

(defmethod preset-play ((p eventplayer) preset dur &rest args)
  (let ((time (or (getf args :time) (now))))
    (cm::sv p
        :playing t
      :end (if dur (+ time dur))
      :dur dur
      :preset preset)
    (funcall #'perform p time args)))

(defmethod preset-play ((p eventplotter) preset dur &rest args)
  (let ((time (or (getf args :time) 0)))
    (cm::sv p
        :playing t
      :end (if dur (+ time dur))
      :dur dur
      :preset preset)
    (funcall #'perform p time args)))

#|

;;;

(defgeneric nperform (player time args))

(defmethod nperform ((player eventplayer) time args)
  "central (tail call) recursive nperform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the index for the next event
is lower than the end index of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (/ (getf args :curr-idx) end))
           (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (setf (getf params :buffer) (lsample-buffer (getf params :lsample)))
            (remf params :dtime)
            (remf params :lsample)
;;;            (break "~S" params)
            (incf (getf params :amp) *master-amp-db*)
            (if *debug* (format t "~&~S" params))
            (apply #'play-buffer-stretch-env-pan-out* params)
;;;            (distributed-play params)
            (if (>= (incf (getf args :curr-idx)) end)
                (setf playing nil)
                (at next #'nperform player next args)))))))

;;; (collect-argvals 0 nil (aref *poolplayer-presets* 15))



(defmethod nperform ((player eventplotter) time args)
    "central (tail call) recursive nperform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the index for the next event
is lower than the end index of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (/ (getf args :curr-idx) end))
           (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (remf params :dtime)
;;;            (break "~S" params)
;;;            (incf (getf params :amp) *master-amp-db*)
            (if *debug* (format t "~&~S" params))
;;;            (break "~S ~a ~a" (cons time params) (getf args :curr-idx) end)
            (push (cons time params) *events*)
;;;            (distributed-play params)
            (if (>= (incf (getf args :curr-idx)) end)
                (setf playing nil)
                (nperform player next args)))))))
|#



#|
(defgeneric npreset-play (player preset &rest args))

(defmethod npreset-play ((p eventplayer) preset &rest args)
  (cm::sv p
      :playing t
    :end (getf args :num)
    :dur nil
    :preset preset)
  (funcall #'nperform p (now) args))

(defmethod npreset-play ((p eventplotter) preset &rest args)
  (cm::sv p
      :playing t
    :end (getf args :num)
    :dur nil
    :preset preset)
;;;  (break "~a, ~a" p args)
  (funcall #'nperform p (getf args :time) args))

|#

