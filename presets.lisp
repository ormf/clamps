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

(defparameter *default-poolplayer-preset* `((:p1 0 :p2 0 :p3 0 :p4 0 :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x) :lsamplefn (r-elt (getf args :g1)) :ampfn (funcall (or (getf args :ampfn) (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6))) x) :transpfn (funcall (getf args :transpfn (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80)))) x) :startfn 0 :endfn 0 :stretchfn (r-exp 1 1) :wwidthfn 123 :attackfn 0 :panfn 0.5 :releasefn 0.01 :outfn (funcall (getf args :outfn #'stereo-out) x))
                                            ,@(repeat 17 nil)))

(defparameter *audio-fn-id-lookup*
  (let ((hash (make-hash-table)))
    (loop for key in '(:preset-form :p1 :p2 :p3 :p4 :dtimefn :lsamplefn :ampfn
                       :transpfn :startfn :endfn :stretchfn
                       :wwidthfn :attackfn :releasefn :panfn :outfn)
       for id from 0
       do (setf (gethash key hash) id))
    hash))

(defun new-poolplayer-preset ()
  (make-array 18 :initial-contents *default-poolplayer-preset*))

;;; (new-poolplayer-preset)
;;; *poolplayer-presets*
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
    `(let ((,preset (aref *poolplayer-presets* ,ref)))
       (progn
         ,@(expand-args preset args))
       (setf (aref ,preset 0) ',args)
       (setf *curr-poolplayer-preset-nr* ,ref)
       ,preset)))

(defun fn-digest-poolplayer-preset (ref args)
  (let ((preset (aref cl-poolplayer::*poolplayer-presets* ref)))
    (loop
       for (key val) on args by #'cddr
       for idx = (cl-poolplayer::get-fn-idx key)
       do (setf (aref preset idx)
                (eval `(lambda (&optional x dur p1 p2 p3 p4 args)
                         (declare (ignorable x dur p1 p2 p3 p4 args))
                          ,val))))
    (setf (aref preset 0) args)
    preset))

;;; (get-fn-idx :ampfn)

#|
;;; implementation as function. The args have to get quoted.

(defun digest-bo-preset (ref args)
  (let ((preset (aref *poolplayer-presets* ref)))
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

(defparameter *poolplayer-presets-file*
  (namestring (merge-pathnames "presets/cl-poolplayer-01.lisp" (asdf:system-source-directory :cl-poolplayer)))) 

;;; (defparameter *audio-presets-file* "presets/schwarm01-audio-presets.lisp")

;;; (setf *poolplayer-presets-file* "presets/schwarm-18-11-18.lisp")

(defparameter *poolplayer-presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (new-poolplayer-preset))))



#| 
(setf *poolplayer-presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (new-poolplayer-preset))))

(setf *default-audio-preset*
      (digest-poolplayer-preset
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

(defparameter *curr-poolplayer-preset-nr* 0)
(defparameter *max-poolplayer-preset-nr* 99)

(defun next-poolplayer-preset ()
  (when (< *curr-poolplayer-preset-nr* *max-poolplayer-preset-nr*)
;;;      (format t "next~%")
      (edit-preset-in-emacs (incf *curr-poolplayer-preset-nr*))))

(defun previous-poolplayer-preset ()
  (when (> *curr-poolplayer-preset-nr* 0)
;;;      (format t "previous~%")
      (edit-preset-in-emacs (decf *curr-poolplayer-preset-nr*))))

(defun show-poolplayer-preset (num)
  (setf *curr-poolplayer-preset-nr* (max (min num *max-poolplayer-preset-nr*) 0))
  (edit-preset-in-emacs *curr-poolplayer-preset-nr*))


#+swank
(defparameter *emcs-conn* swank::*emacs-connection*)

#+swank
(defun define-elisp-code ()
  (let ((swank::*emacs-connection* *emcs-conn*))
    (swank::eval-in-emacs
     `(progn
        (setq poolplayer-preset-file ,(namestring
                                       (merge-pathnames
                                        "curr-preset.lisp"
                                        (asdf:system-source-directory :cl-poolplayer))))
        (find-file poolplayer-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,(namestring
                (merge-pathnames
                 "edit-poolplayer-presets.el"
                 (asdf:system-source-directory :cl-poolplayer))))
        ) t)))

#+slynk
(defun define-elisp-code ()
  (slynk::eval-in-emacs
   `(progn
      (setq poolplayer-preset-file
            ,(namestring
              (merge-pathnames
               "curr-preset.lisp"
               (asdf:system-source-directory :cl-poolplayer))))
      (find-file poolplayer-preset-file)
      (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
      (load ,(namestring
              (merge-pathnames
               "sly-edit-poolplayer-presets.el"
               (asdf:system-source-directory :cl-poolplayer)))))
   t))

#+swank
(defun edit-preset-in-emacs (ref)
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (let ((swank::*emacs-connection* *emcs-conn*))
    (if (numberp ref)
        (swank::eval-in-emacs
         `(edit-poolplayer-preset
           ,(progn
              (in-package :cl-poolplayer)
              (defparameter swank::*send-counter* 0)
              (preset->string ref))
           ,ref) t)
        (swank::eval-in-emacs
         `(save-excursion
           (switch-to-buffer (get-buffer "curr-preset.lisp"))) t))))

#+slynk
(defun edit-preset-in-emacs (ref)
  "send the preset form referenced by <ref> to emacs for display in the
curr-preset.lisp buffer."
  (if (numberp ref)
      (slynk::eval-in-emacs
       `(edit-poolplayer-preset
         ,(progn
            (in-package :cl-poolplayer)
            (preset->string ref))
         ,ref) t)
      (slynk::eval-in-emacs
       `(save-excursion
         (switch-to-buffer (get-buffer "curr-preset.lisp"))) t)))

;;; into init-file: (define-elisp-code)
;;;

;;; (edit-preset-in-emacs 0)

;;; (setf (env-delta (preset-amp-env (aref *poolplayer-presets* 1))) 3)

;;; (sv (aref *poolplayer-presets* 0) :amp-env )

(defun get-preset-form (idx)
  (elt (elt *poolplayer-presets* idx) 0))

(defun get-preset-string (idx)
  (with-output-to-string (out)
    (loop for (key value) on (get-preset-form idx) by #'cddr
          for start = "(" then #\NEWLINE
          do (format out "~a~s ~s" start key value))
    (format out ")")))

;;; (get-preset-string 0)

(defun get-preset-load-form (preset-no)
  (with-output-to-string (out)
    (format out "(digest-poolplayer-preset~%~d~%~A)~%"
            preset-no
            (get-preset-string preset-no))))

;;; (get-preset-load-form 0)

;;; #'save-poolplayer-presets

(defun save-poolplayer-presets (&optional (file *poolplayer-presets-file*))
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (format out "(in-package :cl-poolplayer)~%~%(progn~%")
    (loop for preset across *poolplayer-presets*
          for idx from 0
          do (format out (get-preset-load-form idx)))
    (format out ")~%"))
  (format t "presets written to ~a" file)
  (format nil "presets written to ~a" file))

(defun load-poolplayer-presets (&optional (file *poolplayer-presets-file*))
  (load file))

(defparameter *curr-preset-no* 0)

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

#|
(:dtime :dtime-dev :dur :dur-dev :transp :transp-dev :stretch :stretch-dev
                  :wsize :wsize-dev :amp :amp-dev :inner-attack :inner-attack-dev
                  :inner-release :inner-release-dev :chan :chan-dev)
|#


(defun collect-preset-slots (preset)
  (loop
    for slot in '(:p1 :p2 :p3 :p4 :dtimefn :lsamplefn :ampfn :transpfn :startfn :endfn
                  :stretchfn :wwidthfn :attackfn :panfn :releasefn :outfn)
    collect (preset-print-slot preset slot)))

;;; (collect-preset-slots (aref *poolplayer-presets* 0))

(defun preset->string (ref)
  (let ((preset (aref *poolplayer-presets* ref)))
    (format nil "(digest-poolplayer-preset
          ~a
          (~{~a~^~%~}))"
            ref
            (with-proplist/collecting (slot val) (aref preset 0)
              (format nil ":~a ~s" slot val)))))

;;; (preset->string 0)
