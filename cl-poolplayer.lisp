;;;; cl-poolplayer.lisp

(in-package :cl-poolplayer)

(setf *print-case* :downcase)
;;; (defparameter *pool-hash* (make-hash-table :test #'equal))
(defparameter *events* nil)

#|
(defparameter *audio-fn-id-lookup*
  (let ((hash (make-hash-table)))
    (loop for key in '(:preset-form :p1 :p2 :p3 :p4 :dtimefn :lsamplefn
                       :ampfn :transpfn :startfn :endfn :stretchfn
                       :wwidthfn :attackfn :releasefn :panfn :outfn)
       for id from 0
       do (setf (gethash key hash) id))
    hash))

(defun get-fn-idx (key)
  (gethash key *audio-fn-id-lookup*))
|#

(defun get-preset-fn (preset key)
  (aref preset (get-fn-idx key)))

(defun collect-argvals (x dur preset &rest args)
  "central routine called on each sample to be played. The preset is a
property list containing key/value pairs for each of the synth's
params, where the value is a form containing the body of a function
definition which gets called here within a local scope of x, dur,
p1..p4 and args to produce the parameter value for the current call. x
is the normalized local time within the total duration (dur) of the
preset. The function returns a property list for a call of the synth
issued in the #'perform function."
  (let* ((p1 (apply (get-preset-fn preset :p1) x dur args))
         (p2 (apply (get-preset-fn preset :p2) x dur p1 args))
         (p3 (apply (get-preset-fn preset :p3) x dur p1 p2 args))
         (p4 (apply (get-preset-fn preset :p4) x dur p1 p2 p3 args)))
    (append
     (loop
       for (key fnkey) in '((:dtime :dtimefn) (:lsample :lsamplefn) (:amp :ampfn)
                            (:transp :transpfn) (:start :startfn)
                            (:end :endfn) (:stretch :stretchfn)
                            (:wwidth :wwidthfn) (:attack :attackfn)
                            (:pan :panfn)
                            (:release :releasefn))
       append (list key (apply (get-preset-fn preset fnkey) x dur p1 p2 p3 p4 args)))
     ;;; speaker output is different from the general method to
     ;;; generate args as we want to be able to get phantom sources
     ;;; between arbitrary speakers. We accomplish this by calculating
     ;;; three values: the speaker idx of the two speakers involved
     ;;; and the (normalized) pan between these speakers. The :outfn
     ;;; has to return these three values.
     (multiple-value-bind (out1 out2)
         (apply (get-preset-fn preset :outfn) x dur p1 p2 p3 p4 args)
       (list :out1 out1 :out2 out2)))))

;;; (collect-argvals 0 nil (aref *presets* 0))

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

(defmethod perform ((player eventplayer) time args)
  "central (tail call) recursive perform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the calculated time for the next event
is before the end time of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (normalize-x time end dur))
           (prst (aref *presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (setf (getf params :buffer) (incudine:lsample-buffer (getf params :lsample)))
            (remf params :dtime)
            (remf params :lsample)
;;            (format t "~&~a" params)
            (incf (getf params :amp) *master-amp-db*)
;;            (break "params: ~S" params)
            (if *debug* (format t "~&~S" params))
            (apply #'play-buffer-stretch-env-pan-out* params)
;;;            (distributed-play params)
            (if (and dur (> next end))
                (setf playing nil)
                (at next #'perform player next args)))))))

;;; (collect-argvals 0 nil (aref *presets* 15))

(defmethod perform ((player eventplotter) time args)
  "central (tail call) recursive perform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the calculated time for the next event
is before the end time of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (normalize-x time end dur))
           (prst (aref *presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (remf params :dtime)
;;            (format t "~&~a" params)
            (incf (getf params :amp) *master-amp-db*)
            (push (cons time params) *events*)
            (if (and dur (> next end))
                (setf playing nil)
                (perform player next args)))))))

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
           (prst (aref *presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (setf (getf params :buffer) (incudine:lsample-buffer (getf params :lsample)))
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

;;; (collect-argvals 0 nil (aref *presets* 15))



(defmethod nperform ((player eventplotter) time args)
    "central (tail call) recursive nperform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the index for the next event
is lower than the end index of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (/ (getf args :curr-idx) end))
           (prst (aref *presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
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

(defgeneric stop (p))

(defmethod stop ((p eventplayer))
  (sv p :playing nil))

(defgeneric preset-play (player preset dur &rest args))

(defmethod preset-play ((p eventplayer) preset dur &rest args)
  (sv p
      :playing t
    :end (if dur (+ (now) dur))
    :dur dur
    :preset preset)
  (funcall #'perform p (now) args))

(defmethod preset-play ((p eventplotter) preset dur &rest args)
  (sv p
      :playing t
    :end (if dur (+ (getf args :time) dur))
    :dur dur
    :preset preset)
  (funcall #'perform p (getf args :time) args))


(defgeneric npreset-play (player preset &rest args))

(defmethod npreset-play ((p eventplayer) preset &rest args)
  (sv p
      :playing t
    :end (getf args :num)
    :dur nil
    :preset preset)
  (funcall #'nperform p (now) args))

(defmethod npreset-play ((p eventplotter) preset &rest args)
  (sv p
      :playing t
    :end (getf args :num)
    :dur nil
    :preset preset)
;;;  (break "~a, ~a" p args)
  (funcall #'nperform p (getf args :time) args))
