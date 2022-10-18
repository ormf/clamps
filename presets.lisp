;;; 
;;; presets.lisp
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

;;; a preset defines the parameters of calls to buff-stretch-play.
;;;
;;; The parameters in a preset are given in the form of envelopes
;;; rather than fixed values.
;;;
;;; An event is a metastructure with a duration and a preset slot. The
;;; idea behind an event is that the whole event is like one
;;; note/action on an instrument. This "note" actually consists of
;;; many particles (like grains) which can change their
;;; params/appearance throughout the duration of the event.
;;;
;;; This is accomplished by repeatedly calling buff-stretch-play. The
;;; density of theses calls are determined by the dtime and dteim-dev
;;; parameters of the preset.
;;;
;;; The envelopes of all parameters of a preset are scaled to the
;;; total duration of the event, defining all parameters of the call
;;; to buff-stretch-play at any moment over the course of the event.
;;;
;;; As there are two layers, 1. the calls to buff-stretch-play and
;;; 2. the call to play an event, it is important not to confuse the
;;; two. E.g. the amp envelope of the event defines the succession of
;;; individual amp values for each buff-stretch-play call over the
;;; course of an event, whereas each individual buff-stretch-play also
;;; has an amp-envelope, defined by "suswidth" and "suspan" parameters
;;; of the preset (which are also defined as envelopes and thus can
;;; change over the course of an event).

#|
(defstruct preset
  (dtime (make-env :start 0.1 :delta 1 :type :exp))
  (dtime-dev (make-env :start 1.2 :delta 1 :type :exp))
  (dur (make-env :start 3 :delta 1 :type :exp))
  (dur-dev (make-env :start 1 :delta 1 :type :exp))
  (transp (make-env))
  (transp-dev (make-env))
  (stretch (make-env :start 1 :delta 1 :type :exp))
  (stretch-dev (make-env :start 1 :delta 1 :type :exp))
  (wsize (make-env :start 123 :delta 1 :type :exp))
  (wsize-dev (make-env :start 1 :delta 1 :type :exp))
  (amp (make-env))
  (amp-dev (make-env))
  (inner-attack (make-env)) ;;; attack of the env of a single buff-stretch-play call
  (inner-attack-dev (make-env :start 1 :delta 1 :type :exp))
  (inner-release (make-env :start 0.01))  ;;; release of the env of a single buff-stretch-play call
  (inner-release-dev (make-env :start 1 :delta 1 :type :exp))
  (chan 0)
  (chan-dev (make-env)))
|#

(defparameter *default-audio-preset* '(nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

(defparameter *audio-fn-id-lookup*
  (let ((hash (make-hash-table)))
    (loop for key in '(:preset-form :p1 :p2 :p3 :p4 :dtimefn :lsamplefn :ampfn :transpfn :startfn :endfn :stretchfn
                       :wwidthfn :attackfn :releasefn :panfn :outfn)
       for id from 0
       do (setf (gethash key hash) id))
    hash))

(defun new-audio-preset ()
  (make-array 17 :initial-contents *default-audio-preset*))

;;; (new-audio-preset)
;;; *presets*
(defun get-fn-idx (key)
  (gethash key *audio-fn-id-lookup*))

(defun expand-args (preset args)
;;;  (format t "~&args: ~a" args)
  (loop
    for (key val) on (canonisize-arg-list args) by #'cddr
    for idx = (get-fn-idx key)
    for n from 3
    collect `(setf (aref ,preset ,idx)
                   (lambda ,(append (subseq '(&optional x dur p1 p2 p3 p4) 0 (min n 7)) '(args))
                     (declare (ignorable ,@(append (subseq '(&optional x dur p1 p2 p3 p4) 1 (min n 7)) '(args))))
                     ,val))))

(defun canonisize-arg-list (args)
  "make sure the args property list begins with the properties :p1 to
:p4 in order."
  (append (ou:get-props-list args '(:p1 :p2 :p3 :p4) :force-all t)
          (ou:delete-props args :p1 :p2 :p3 :p4)))

;;; (let ((n 5)) (append (subseq '(&optional x dur p1 p2 p3 p4) 0 (min n 7)) '(args)))

;;; (canonisize-arg-list '(:p3 13 :p2 1 :transpfn (lambda (x) x) :p4 22))

(defmacro digest-poolplayer-preset (ref args)
  (let ((preset (gensym "preset")))
    `(let ((,preset (aref *presets* ,ref)))
       (progn
         ,@(expand-args preset args))
       (setf (aref ,preset 0) ',args)
       ,preset)))

;;; (get-fn-idx :ampfn)

#|
;;; implementation as function. The args have to get quoted.

(defun digest-bo-preset (ref args)
  (let ((preset (aref *presets* ref)))
    (loop
       for (key val) on args by #'cddr
       for idx = (get-fn-idx key)
       do (setf (aref preset idx)
                (eval `(lambda (&optional x dur p1 p2 p3 p4 args)
                         (declare (ignorable x dur p1 p2 p3 p4 args))
                          ,val))))
    (setf (aref preset 0) args)
    preset))
|#

;;; (make-env)

;;; (make-preset)

(defparameter *presets-file* "presets/big-orchestra01.lisp")
;;; (defparameter *audio-presets-file* "presets/schwarm01-audio-presets.lisp")

;;; (setf *presets-file* "presets/schwarm-18-11-18.lisp")

(defparameter *presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (new-audio-preset))))

#| 
(setf *default-audio-preset*
      (digest-bo-preset
       0
       `(:p1 0
         :p2 0
         :p3 0
         :p4 0
         :dtimefn 0.5
         :lsamplefn (r-elt *buffers*)
         :ampfn (n-exp 0 1 2)
         :transpfn (r-exp 1 2)
         :startfn 0
         :endfn 0
         :stretchfn (r-exp 1 1)
         :wwidthfn 123
         :attackfn 0
         :releasefn 0.01
         :outfn 0)))
|#

(setf *presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (new-audio-preset))))

;;; (setf (env-delta (preset-amp-env (aref *presets* 1))) 3)

;;; (sv (aref *presets* 0) :amp-env )

(defun get-preset-form (idx)
  (elt (elt *presets* idx) 0))

(defun get-preset-string (idx)
  (with-output-to-string (out)
    (loop for (key value) on (get-preset-form idx) by #'cddr
          for start = "'(" then #\NEWLINE
          do (format out "~a~s ~s" start key value))
    (format out ")")))

;;; (get-preset-string 1)

(defun get-preset-load-form (preset-no)
  (with-output-to-string (out)
    (format out "(digest-bo-preset~%~d~%~A)~%"
            preset-no
            (get-preset-string preset-no))))

;;; (get-preset-load-form 0)

;;; #'save-presets

(defun save-presets (&key (file *presets-file*))
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (format out "(in-package :cl-poolplayer)~%~%(progn~%")
    (loop for preset across *presets*
          for idx from 0
          do (format out (get-preset-load-form idx)))
    (format out ")~%"))
  (format t "presets written to ~a" file)
  (format nil "presets written to ~a" file))

;;; (save-presets :file "presets/test02.lisp")

(defun load-presets (file)
  (load file))

;;; (load-presets "presets/big-orchestra01.lisp")

(defparameter *curr-preset-no* 0)
(defparameter *curr-audio-preset-no* 0)

;;; tmp storage for all bound cc-fns in running preset. Used for
;;; suspending current pending actions when changing a preset before
;;; reassignment.

(defparameter *curr-cc-fns* nil)

(defun preset-print-slot (preset slot)
  (format nil ":~a ~a" (string-downcase slot)
        (let ((p-sv (slot-value preset (keyword->symbol slot))))
          (case (type-of p-sv)
            (env (format nil "(~{~a~^ ~})"
                         (loop
                           with env = p-sv
                           for elem in '(:start :delta :attack :release :type)
                           collect (format nil ":~a ~a" (string-downcase elem)
                                           (let ((v (slot-value env (keyword->symbol elem))))
                                             (if (symbolp v) (format nil ":~a" v) v))))))
            (otherwise p-sv)))))

(defun collect-preset-slots (preset)
  (loop
    for slot in '(:dtime :dtime-dev :dur :dur-dev :transp :transp-dev :stretch :stretch-dev
                  :wsize :wsize-dev :amp :amp-dev :inner-attack :inner-attack-dev
                  :inner-release :inner-release-dev :chan :chan-dev)
    collect (preset-print-slot preset slot)))

;;; (collect-preset-slots (aref *presets* 0))

(defun preset->string (ref)
  (let ((preset (aref *presets* ref)))
    (format nil "(digest-bo-preset
          ~a
          (~{~a~^~%~}))"
            ref
            (loop
              for (slot val) on (aref preset 0) by #'cddr
              collect (format nil ":~a ~s" slot val)))))

;;; (preset->string 0)

(defparameter *emcs-conn* swank::*emacs-connection*)

(defun edit-preset-in-emacs (ref)
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-big-orchestra-preset
           ,(progn
              (in-package :cl-poolplayer)
              (defparameter swank::*send-counter* 0)
              (preset->string ref))
           ,(format nil "~a" ref))
         t)
        (swank::eval-in-emacs `(edit-big-orchestra-preset
                                ,(preset->string ref)
                                ,(format nil "~a" *curr-preset-no*)) t))))

;;; (edit-preset-in-emacs 0)

(defun previous-preset ()
  (let ((next-no (max 0 (1- *curr-preset-no*))))
    (if (/= next-no *curr-preset-no*)
        (progn
          (setf *curr-preset-no* next-no)
;;;          (qt:emit-signal (find-gui :pv1) "setPreset(int)" *curr-preset-no*)
          (edit-preset-in-emacs *curr-preset-no*)))
    *curr-preset-no*))

;;; (edit-preset-in-emacs 0)


#|
(next-preset)
(qt:emit-signal (find-gui :pv1) "setPreset(int)" 3)
(previous-preset)

                                      ;
|#

(defun next-preset ()
  (let ((next-no (min 127 (1+ *curr-preset-no*))))
    (if (/= next-no *curr-preset-no*)
        (progn
          (setf *curr-preset-no* next-no)
          ;; (qt:emit-signal (find-gui :pv1) "setPreset(int)" *curr-preset-no*)
          (edit-preset-in-emacs *curr-preset-no*)))
    *curr-preset-no*))

(defun goto-preset (num)
  (let ((next-no (min 127 num)))
    (if (/= next-no *curr-preset-no*)
        (progn
          (setf *curr-preset-no* next-no)
          ;; (qt:emit-signal (find-gui :pv1) "setPreset(int)" *curr-preset-no*)
          (edit-preset-in-emacs *curr-preset-no*)))
    *curr-preset-no*))

#|

(defun digest-bo-preset (ref defs)
  (setf (aref *presets* ref)
        (apply #'make-preset
               (loop
                 for (slot val) on defs by #'cddr
                 append (list (intern (string-upcase (format nil "~a" slot)) :keyword)
                               (if (consp val)
                                   (apply #'make-env val)
                                   val))))))

  (defun set-fixed-cc-fns (nk2-chan)
  (setf (aref *cc-fns* nk2-chan 58)
  (lambda (d2)
  (if (= d2 127)
  (previous-preset))))
  (setf (aref *cc-fns* nk2-chan 43)
  (lambda (d2)
  (declare (ignore d2))
  (load-current-preset)))
  (setf (aref *cc-fns* nk2-chan 46)
  (lambda (d2)
  (if (= d2 127)
  (edit-preset-in-emacs *curr-preset-no*))))

  (setf (aref *cc-fns* nk2-chan 59)
  (lambda (d2)
  (if (= d2 127)
  (next-preset))))

  (setf (aref *cc-fns* nk2-chan 45)
  (lambda (d2)
  (if (= d2 127)
  (load-current-preset))))
  
  (setf (aref *cc-fns* nk2-chan 61)
  (lambda (d2)
  (if (= d2 127)
  (previous-audio-preset))))

  (setf (aref *cc-fns* nk2-chan 60)
  (lambda (d2)
  (if (= d2 127)
  (edit-audio-preset-in-emacs *curr-audio-preset-no*))))
  
  (setf (aref *cc-fns* nk2-chan 62)
  (lambda (d2)
  (if (= d2 127)
  (next-audio-preset))))
  (setf (aref *cc-fns* nk2-chan 42)
  (lambda (d2)
  (declare (ignore d2))
  (cl-boids-gpu::reshuffle-life cl-boids-gpu::*win* :regular nil))))
|#


#|

(defmacro nk2-ref (ref)
  `(aref *cc-state* *nk2-chan* ,ref))
|#


#|
(defun edit-audio-preset-in-emacs (ref)
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-flock-audio-preset
           ,(progn
              (in-package :luftstrom-display)
              (get-audio-preset-load-form ref))
           ,(format nil "~a" ref))
                              t))))

(defun view-audio-preset-in-emacs (ref)
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(view-flock-audio-preset
           ,(progn
              (in-package :luftstrom-display)
              (get-audio-preset-load-form ref))
           ,(format nil "~a" ref))
         t))))

|#
