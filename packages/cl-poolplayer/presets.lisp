;;;
;;; presets.lisp
;;;
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
;;; density of theses calls are determined by the :dtimefn of the
;;; preset.
;;;
;;; The envelopes of all parameters of a preset are scaled to the
;;; total duration of the event, defining all parameters of the call
;;; to buff-stretch-play at any moment over the course of the event.
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

;;; a preset struct only consists of a form, its digested :dtimefn and its digested params-fn.

(defstruct (poolplayer-preset (:conc-name nil))
  (preset-form)
  (dtime-fn)
  (params-fn))

(defparameter *poolplayer-presets-file*
  (namestring (merge-pathnames "presets/cl-poolplayer-01.lisp"
                               (asdf:system-source-directory :cl-poolplayer)))) 


(defparameter *curr-poolplayer-preset-no* 0)
(defparameter *curr-preset-no* 0)
(defparameter *max-poolplayer-preset-no* 99)


(defparameter *poolplayer-presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (make-poolplayer-preset))))

;;; Use the new definition of poolplayer-preset for *poolplayer-presets*
;;; TODO: Incorporate into cl-poolplayer.

(setf *poolplayer-presets*
  (coerce (loop repeat 100 collect (make-poolplayer-preset)) 'vector))

#|
;;; deprecated:

(defparameter *audio-fn-id-lookup*
  (let ((hash (make-hash-table)))
    (loop for key in '(:preset-form :p1 :p2 :p3 :p4 :dtimefn :lsamplefn :ampfn
                       :transpfn :startfn :endfn :stretchfn
                       :wwidthfn :attackfn :releasefn :panfn :outfn)
          for id from 0
          do (setf (gethash key hash) id))
    hash))
|#

(defparameter *param-lookup*
  (mapcar (lambda (key) (list (make-keyword (format nil "~afn" key))
                         (intern (format nil "~:@(~afn~)" key))
                         key ))
          '(:buffer :transp :amp :dy :start :end :stretch
            :wwidth :attack :release :pan :out1 :out2))
  "List of all param-fn keywords, param-fn symbols and param keywords.")

(defparameter *default-param-fns*
  (let ((hash-table (make-hash-table)))
    (loop for (key val) on
          '(:dtimefn 0.3
            :bufferfn nil
            :transpfn 60
            :ampfn 0
            :dyfn 0
            :startfn 0
            :endfn 0
            :stretchfn 1
            :wwidthfn 137
            :attackfn 0
            :releasefn 0.01
            :panfn 0.5
            :out1fn 0
            :out2fn 1)
          by #'cddr
          do (setf (gethash key hash-table) val))
    hash-table)
  "Defaults for the param fns."
  )

(defun get-param-fns (form)
  "Return a list of function definition forms for all param-fns of a
poolplayer-preset in a list. The list is used in digest-form-to-preset
for labels bindings. If /form/ doesn't contain an entry for the
param-fn, it is retrieved from *default-param-fns*."
  (loop for (fnkey fnsym key) in *param-lookup*
        collect `(,fnsym (x &optional dur args)
                         (declare (ignorable x dur args )
                                  (type (or null number) dur))
                         (let ((outer-form (getf args ,fnkey)))
                           (if outer-form
                               (funcall
                                (eval
                                 `(lambda (x &optional dur args)
                                    (declare (ignorable x dur args))
                                    ,outer-form))
                                x dur args)
                               ,(getf form fnkey (gethash fnkey *default-param-fns*)))))))

;;; (get-param-fns '(:transpfn 0 :ampfn (n-lin x 0 -12)))


(defun binding-syms (bindings)
  "Return the binding symbols of /bindings/ in a list. /bindings/ has the
same syntax as an argument of let."
  (mapcar (lambda (x) (if (consp x) (first x) x)) bindings))

(defun dtime-fn-form (form)
  "Return the lambda form for the dtime-fn from /form/ as a quoted list."
  `(lambda (x &optional dur &rest args)
     (declare (ignorable x dur args)
              (type (or null number) dur))
     (let* ,(getf form :bindings)
       (declare (ignorable ,@(binding-syms (getf form :bindings)))
                (type (or null number) dur))
       (let ((outer-form (getf args :dtimefn)))
                           (if outer-form
                               (funcall
                                (eval
                                 `(lambda (x &optional dur args)
                                    (declare (ignorable x dur args))
                                    ,outer-form))
                                x dur args)
                               ,(getf form :dtimefn (gethash :dtimfn *default-param-fns*)))))))

(defun params-fn-form (form)
  "Return the lambda form for the parmas-fn from /form/ as a quoted list."
  `(lambda (x dur &rest args)
     (let* ,(getf form :bindings)
       (declare (ignorable ,@(binding-syms (getf form :bindings))))
       (labels ,(get-param-fns form)
         (list ,@(loop
                   for (fnkey fnsym key) in *param-lookup*
                   append `(,key (,fnsym x dur args))))))))

(defmacro digest-form-to-preset (preset-no form)
  "A call to this macro will trigger compiling the dtime-fn and params-fn
definitions of /form/ and store the compiled functions and /form/ into
the *poolplayer-preset* with index /preset-no/."
  `(progn
     (incudine.util:msg :info "Compiling form to preset ~a" ,preset-no)
     (setf (preset-form (aref *poolplayer-presets* ,preset-no)) ',form)
     (setf (dtime-fn (aref *poolplayer-presets* ,preset-no))
           ,(dtime-fn-form form))
     (setf (params-fn (aref *poolplayer-presets* ,preset-no))
           ,(params-fn-form form))
     (aref *poolplayer-presets* ,preset-no)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           File IO:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro set-poolplayer-preset-form (idx form)
  `(setf (preset-form (aref *poolplayer-presets* ,idx)) ',form))

(defun get-preset-load-form (preset-no)
  (with-output-to-string (out)
    (format out "(set-poolplayer-preset-form~%~d~%~A)~%"
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

;;; load-poolplayer-presets

(defun load-poolplayer-presets (&optional (file *poolplayer-presets-file*))
  (load file))

(defun preset->digest-string (ref)
  (let ((preset (aref *poolplayer-presets* ref)))
    (format nil "(digest-form-to-preset
          ~a
          (~{~a~^~%~}))"
            ref
            (do-proplist/collecting (slot val) (preset-form preset)
              (format nil ":~a ~s" slot val)))))

;;; (format t (preset->digest-string 0))




;;; Emacs "GUI":

#+slynk
(defparameter slynk::*send-counter* 0)

#-slynk
(defparameter *emcs-conn* swank::*emacs-connection*)

#+slynk
(defparameter *emcs-conn* slynk::*emacs-connection*)

(defun next-poolplayer-preset ()
  (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
    (when (< *curr-poolplayer-preset-no* *max-poolplayer-preset-no*)
      (incudine.util:msg :info "next")
      (edit-preset-in-emacs (incf *curr-poolplayer-preset-no*)))))

(defun previous-poolplayer-preset ()
  (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
    (when (> *curr-poolplayer-preset-no* 0)
      (incudine.util:msg :info "previous")
      (edit-preset-in-emacs (decf *curr-poolplayer-preset-no*)))))

(defun show-poolplayer-preset (num)
  (let ((new (max (min (round num) *max-poolplayer-preset-no*) 0)))
    (when (/= new *curr-poolplayer-preset-no*)
      (setf *curr-poolplayer-preset-no* new)
      (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
        (edit-preset-in-emacs *curr-poolplayer-preset-no*)))))

#-slynk
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
        )
     t)))

#+slynk
(defun define-elisp-code (&key (basedir (asdf:system-source-directory :cl-poolplayer)))
  (let ((slynk::*emacs-connection* *emcs-conn*))
    (slynk::eval-in-emacs
     `(progn
        (setq poolplayer-preset-file
              ,(namestring
                (merge-pathnames
                 "curr-preset.lisp"
                 basedir)))
        (find-file poolplayer-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,(namestring
                (merge-pathnames
                 "sly-edit-poolplayer-presets.el" basedir))))
     t)))

#-slynk
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
            (defparameter slynk::*send-counter* 0)
            (preset->digest-string ref))
         ,*curr-preset-no*) t)
      (slynk::eval-in-emacs
       `(save-excursion
         (switch-to-buffer (get-buffer "curr-preset.lisp"))) t)))

(defun init-poolplayer (presets-file)
  "Set poolplayer presets file to /presets-file/, load all presets, set
the *curr-preset* to 0 and display it in emacs buffer."
  (setf *poolplayer-presets-file*
        (namestring presets-file))
  (load-poolplayer-presets)
  (setf *curr-preset-no* 0)
  (uiop:run-program "/usr/bin/touch /tmp/curr-preset.lisp")
  (cl-poolplayer::define-elisp-code :basedir (pathname "/tmp/")))

;;; into init-file: (define-elisp-code)
;;;

;;; (edit-preset-in-emacs 0)

(defun get-preset-form (idx)
  (cl-poolplayer::preset-form (elt *poolplayer-presets* idx)))

(defun get-preset-string (idx)
  (with-output-to-string (out)
    (loop for (key value) on (get-preset-form idx) by #'cddr
          for start = "(" then #\NEWLINE
          do (format out "~a~s ~s" start key value))
    (format out ")")))

