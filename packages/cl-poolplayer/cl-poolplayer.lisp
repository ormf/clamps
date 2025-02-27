;;;; cl-poolplayer.lisp

(in-package :cl-poolplayer)

(setf *print-case* :downcase)
;;; (defparameter *pool-hash* (make-hash-table :test #'equal))
(defparameter *poolplayer-events* nil)

(defun get-preset-fn (preset key)
  (aref preset (get-fn-idx key)))

(defun collect-argvals (x dur preset &rest args)
  "Central routine called on each sample to be played. This routine
returns all the arguments for a call to the synth in a property list.

/x/ is the normalized time of the sample in the interval [0..dur].

/preset/ is a vector of functions, one for each synth argument.

The first four functions of /preset/ are special: They are used to
calculate the params p1, p2, p3 and p4 before any of the synth
arguments are calculated. Each of the functions for the synth
arguments are then called with x, dur and p1..p4 as arguments. That
way, p1..p4 can be referenced as global variables for the calculation
of synth arguments within their respective functions."
  (let* ((p1 (apply (get-preset-fn preset :p1) x dur args)) ;;; calc "global" vars
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
     ;;; between arbitrary speakers. We accomplish this by returning
     ;;; two index values of the outputs to use with the :outfn. The
     ;;; previously calculated :pan will pan between those two
     ;;; outputs.
     (multiple-value-bind (out1 out2)
         (apply (get-preset-fn preset :outfn) x dur p1 p2 p3 p4 args)
       (list :out1 out1 :out2 out2)))))

;;; (collect-argvals 0 nil (aref *poolplayer-presets* 0) (list :poolposfn (lambda (x) (random 1.0))))

(get-preset-fn (aref *poolplayer-presets* 0) :outfn)

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

(defun get-evts (time dur preset &rest args)
  "central perform routine used by #'preset-play: It calculates params
according to the preset definition used by the player and pushes them
as property list with prepended time to result. It then reschedules
itself in case the calculated time for the next event is before the
end time of the player's life cycle. Otherwise it just sets the
'playing slot of the player to nil and returns the accumulated
result."
  (let ((end (+ time dur)))
    (labels ((inner (time args)
               (let* ((x (normalize-x time end dur))
                      (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
                      (params (collect-argvals x dur prst args)))
                 ;;        (format t "x: ~a, preset: ~a" x prst)
                 ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
                 (let* ((next (+ time (getf params :dtime)))
                        (keynum (+ (lsample-keynum (getf params :lsample)) (getf params :transp))))
                   (remf params :dtime)
                   (setf (getf params :keynum) keynum)
                   (remf params :transp)
                   ;;            (format t "~&~a" params)
                   (incf (getf params :amp) *master-amp-db*)
                   (cons
                    (apply #'make-instance 'cm::poolevt
                           :time (float time 1.0)
                           params)
                    (if (<= next end)
                        (inner next args)))))))
      (inner time args))))

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
           (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (setf (getf params :buffer) (lsample-buffer (getf params :lsample)))
            (remf params :dtime)
            (incf (getf params :amp) *master-amp-db*)
            (push (cons (now) (copy-list params)) *poolplayer-events*)
            (remf params :lsample)
;;            (format t "~&~a" params)
            (incudine.util:msg :info "params: ~a ~a ~S" next end params)
            (apply #'play-buffer-stretch-env-pan-out* :env *env1* params)
;;;            (distributed-play params)
            (if (and dur (> next end))
                (setf playing nil)
                (at next #'perform player next args)))))))

#|
(defmethod perform ((player eventplayer) time args)
  "central (tail call) recursive perform routine used by
#'preset-play: It calculates params according to the preset definition
used by the player and calls #'play-buffer-stretch-env-out on them. It
then reschedules itself in case the calculated time for the next event
is before the end time of the player's life cycle. Otherwise it just
sets the 'playing slot of the player to nil and returns."
  (with-slots (playing preset start end dur) player
    (let* ((x (normalize-x time end dur))
           (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
           (params (collect-argvals x dur prst args)))
      ;;        (format t "x: ~a, preset: ~a" x prst)
      ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
      (if playing
          (let* ((next (+ time (getf params :dtime))))
            (setf (getf params :buffer) (lsample-buffer (getf params :lsample)))
            (remf params :dtime)
            (remf params :lsample)
;;            (format t "~&~a" params)
            (incf (getf params :amp) *master-amp-db*)
;;;            (break "params: ~S" params)
            (if *debug* (format t "~&~S" params))
            (apply #'play-buffer-stretch-env-pan-out* :env *env1* params)
;;;            (distributed-play params)
            (if (and dur (> next end))
                (setf playing nil)
                (at next #'perform player next args)))))))
|#
;;; (collect-argvals 0 nil (aref *poolplayer-presets* 0))

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
               (with-slots (playing preset start end dur) player
                 (let* ((x (normalize-x time end dur))
                        (prst (aref *poolplayer-presets* (if (= -1 preset) *curr-preset-no* preset))) ;;; if preset is -1 use *curr-preset*
                        (params (collect-argvals x dur prst args)))
                   ;;        (format t "x: ~a, preset: ~a" x prst)
                   ;;        (format t "end: ~a, time: ~a, dur: ~a, x: ~a, playing: ~a~%" end time dur x playing)
                   (if playing
                       (let* ((next (+ time (getf params :dtime))))
                         (remf params :dtime)
                         ;;            (format t "~&~a" params)
                         (incf (getf params :amp) *master-amp-db*)
                         (push (cons time params) result)
                         (if (and dur (> next end))
                             (setf playing nil)
                             (inner player next args))))))))
      (inner player time args)
      (reverse result))))

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

;;; should be moved to cm-poolplayer:


