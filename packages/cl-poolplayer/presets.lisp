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

(defvar *poolplayer-presets-file*
  (namestring (merge-pathnames "presets/cl-poolplayer-01.lisp"
                               (asdf:system-source-directory :cl-poolplayer)))
  "Pathname of the storage of poolplayer presets.

@See-also
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  ) 


(defvar *curr-poolplayer-preset-no* 0)
;;; (defparameter *curr-preset-no* 0)
(defvar *max-poolplayer-preset-no* 99)


(defvar *poolplayer-presets*
  (make-array
   100
   :element-type 'vector
   :initial-contents
   (loop for i below 100 collect (make-poolplayer-preset)))
  "Storage of the ~form~, ~dtime-fn~ and ~params-fn~ of all poolplayer presets.

@See-also
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
")

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
          '(:lsample :transp :amp :dy :start :end :stretch
            :wwidth :attack :release :pan :out1 :out2 :play))
  "List of all param-fn keywords, param-fn symbols and param keywords of a poolplayer preset.")

(defparameter *default-param-fns*
  (let ((hash-table (make-hash-table)))
    (loop for (key val) on
          '(:dtimefn 0.3
            :lsamplefn nil
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
            :out2fn 1
	    :playfn #'of-incudine-dsps::play-buffer-stretch-env-pan-bus-out*)
          by #'cddr
          do (setf (gethash key hash-table) val))
    hash-table)
  "Defaults for the param fns of a poolplayer preset."
  )

(defun get-param-fns (form)
  "Return a list of function definition forms for all param-fns of a
poolplayer preset in a list. The list is used in digest-form-to-preset
for labels bindings. If /form/ doesn't contain an entry for the
param-fn, it is retrieved from *default-param-fns*."
  (loop for (fnkey fnsym key) in *param-lookup*
        collect `(,fnsym ()
                         (let ((outer-form (getf inits ,fnkey)))
                           (if outer-form
                               (funcall
                                (eval
                                 `(lambda (x &optional dtime dur idx &rest inits)
                                    (declare (ignorable x dtime dur inits))
                                    ,outer-form))
                                x dtime dur idx inits)
                               ,(getf form fnkey (gethash fnkey *default-param-fns*)))))))

;;; (get-param-fns '(:transpfn 0 :ampfn (n-lin x 0 -12)))


(defun binding-syms (bindings)
  "Return the binding symbols of /bindings/ in a list. /bindings/ has the
same syntax as an argument of let."
  (mapcar (lambda (x) (if (consp x) (first x) x)) bindings))

(defun dtime-fn-form (form)
  "Return the lambda form for the dtime-fn from /form/ as a quoted list."
  `(lambda (x &optional dur idx &rest inits)
     (declare (ignorable x dur idx inits)
              (type (or null number) dur))
     (let* ,(getf form :bindings)
       (declare (ignorable ,@(binding-syms (getf form :bindings)))
                (type (or null number) dur))
       (let ((outer-form (getf inits :dtimefn)))
                           (if outer-form
                               (funcall
                                (eval
                                 `(lambda (x &optional dur idx inits)
                                    (declare (ignorable x dur idx inits))
                                    ,outer-form))
                                x dur inits)
                               ,(getf form :dtimefn (gethash :dtimefn *default-param-fns*)))))))


#|
(defun params-fn-form (form)
  "Return the lambda form for the params-fn from /form/ as a quoted list."
  `(lambda (x dur &rest inits)
     (let* ,(getf form :bindings)
       (declare (ignorable ,@(binding-syms (getf form :bindings))))
       (labels ,(get-param-fns form)
         (let ((lsample (lsamplefn x dur inits)))
           (append `(:lsample ,lsample :buffer ,(lsample-buffer lsample))
                   (list
                    ,@(loop
                      for (fnkey fnsym key) in (remove-if (lambda (x) (member x '(:buffer :lsample)))
                                                          *param-lookup*
                                                          :key #'third)

                      append `(,key (,fnsym x dur inits))))))))))
|#

(defun params-fn-form (form)
  "Return the lambda form for the params-fn from /form/ as a quoted list."
  `(lambda (x dtime dur idx &rest inits)
     (declare (ignorable x dtime dur idx inits)
              (type (or null number) dur))
     (let* ,(getf form :bindings)
       (labels ,(get-param-fns form)
         (list
          ,@(loop
              for (fnkey fnsym key) in *param-lookup*
              append `(,key (,fnsym))))))))

(defmacro digest-form-to-preset (preset-no form)
  "A call to this macro will trigger compiling the ~dtime-fn~ and ~params-fn~q
definitions of /form/ and store the compiled functions and /form/ into
the ~*poolplayer-presets*~ variable with index /preset-no/.

@Arguments
preset-no - Number in the range [0.99] denoting the index of the preset to compile into.
form - Preset form to digest.

@See-also
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  `(progn
     (incudine.util:msg :info "Compiling form to preset ~a" ,preset-no)
     (setf (preset-form (aref *poolplayer-presets* ,preset-no)) ',form)
     (setf (dtime-fn (aref *poolplayer-presets* ,preset-no))
           ,(dtime-fn-form form))
     ;; (setf (params-fn (aref *poolplayer-presets* ,preset-no))
     ;;       ,(params-fn-form form))
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
  "Save *poolplayer-presets* to /file/.

@Arguments
file - Pathname or String denoting the filename of the poolplayer presets file.

@See-also
digest-form-to-preset
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
show-poolplayer-preset
"
  (with-open-file (out file :direction :output
                            :if-exists :supersede)
    (format out "(in-package :clamps)~%~%(progn~%")
    (loop for preset across *poolplayer-presets*
          for idx from 0
          do (format out (get-preset-load-form idx)))
    (format out ")~%"))
  (format t "presets written to ~a" file)
  (format nil "presets written to ~a" file))

;;; load-poolplayer-presets

(defun load-poolplayer-presets (&optional (file *poolplayer-presets-file*))
  "Load *poolplayer-presets* from /file/.

@Arguments
file - Pathname or String denoting the filename of the poolplayer presets file.

@See-also
digest-form-to-preset
edit-preset-in-emacs
init-poolplayer
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
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
  "Display next poolplayer preset in the ~curr-preset.lisp~ buffer. Bound
to the ~<M-Right>~ keyboard shortcut.

@See-also
digest-form-to-preset
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
    (when (< *curr-poolplayer-preset-no* *max-poolplayer-preset-no*)
      (incudine.util:msg :info "next")
      (edit-preset-in-emacs (incf *curr-poolplayer-preset-no*)))))

(defun previous-poolplayer-preset ()
  "Display previous poolplayer preset in the ~curr-preset.lisp~ buffer. Bound
to the ~<M-Left>~ keyboard shortcut.

@See-also
digest-form-to-preset
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
save-poolplayer-presets
show-poolplayer-preset
"  (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
    (when (> *curr-poolplayer-preset-no* 0)
      (incudine.util:msg :info "previous")
      (edit-preset-in-emacs (decf *curr-poolplayer-preset-no*)))))

(defun show-poolplayer-preset (num)
  "Show preset /num/ in the ~curr-preset.lisp~ buffer.

@Arguments
num - Non-negative Integer denoting the index of *poolplayer-presets* to display.

@See-also
digest-form-to-preset
edit-preset-in-emacs
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  (let ((new (max (min (round num) *max-poolplayer-preset-no*) 0)))
    (when (/= new *curr-poolplayer-preset-no*)
      (setf *curr-poolplayer-preset-no* new)
      (let ((slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
        (edit-preset-in-emacs *curr-poolplayer-preset-no*)))))

#-slynk
(defun define-poolplayer-elisp-code (&key (elisp-basedir (asdf:system-source-directory :cl-poolplayer)) (curr-preset-file "/tmp/curr-preset.lisp"))
  (let ((swank::*emacs-connection* *emcs-conn*))
    (swank::eval-in-emacs
     `(progn
        (setq poolplayer-preset-file ,curr-preset-file)
        (find-file poolplayer-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,(namestring
                (merge-pathnames
                 "sly-edit-poolplayer-presets.el" elisp-basedir))))
     t)))

#+slynk
(defun init-poolplayer-elisp-code (elisp-file)
  "Init the emacs interface for poolplayer preset editing using /elisp-file/.
@Arguments
elisp-file - String denoting the elisp file to load.
"
  (let ((curr-preset-file "/tmp/curr-preset.lisp")
        (slynk::*emacs-connection* (or slynk::*emacs-connection* *emcs-conn*)))
    (uiop:run-program "/usr/bin/touch /tmp/curr-preset.lisp")
    (slynk::eval-in-emacs
     `(progn
        (setq poolplayer-preset-file ,curr-preset-file)
        (find-file ,curr-preset-file)
        (set-window-dedicated-p (get-buffer-window "curr-preset.lisp" t) t)
        (load ,elisp-file))
     t)
    (edit-preset-in-emacs 0)))

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
  "Send the poolplayer preset form referenced by /ref/ to emacs for
display in the ~curr-preset.lisp~ buffer.

@Arguments
ref - Non-negative integer denoting the index of *poolplayer-presets*

@See-also
digest-form-to-preset
init-poolplayer
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  (when (numberp ref)
    (setf *curr-poolplayer-preset-no* ref)
    (slynk::eval-in-emacs
     `(edit-poolplayer-preset
       ,(progn
          (in-package :cl-poolplayer)
          (defparameter slynk::*send-counter* 0)
          (preset->digest-string ref))
       ,*curr-poolplayer-preset-no*)
     t)
    (slynk::eval-in-emacs
     `(save-excursion
       (switch-to-buffer (get-buffer "curr-preset.lisp"))) t)))

(defun init-poolplayer (&optional presets-file elisp-file)
  "Set <<*poolplayer-presets*>> by loading /presets-file/, digesting all
presets and display the preset at index 0 in an emacs buffer linked to
the file ~/tmp/curr-preset.lisp~. Load the elisp definitions for
preset editing from /elisp-file/.

@Arguments
presets-file - Pathname or string denoting the presets file to load. The default copies cl-poolplayer/presets/cl-poolplayer-defaults-presets.lisp from the clamps packages directory to /tmp/poolplayer-presets.lisp and uses the latter file.
elisp-file - Pathname or string denoting the Emacs lisp configuration file for the emacs preset editor. Defaults to cl-poolplayer/sly-edit-poolplayer-presets.el

digest-form-to-preset
edit-preset-in-emacs
load-poolplayer-presets
next-poolplayer-preset
*poolplayer-presets*
*poolplayer-presets-file*
previous-poolplayer-preset
save-poolplayer-presets
show-poolplayer-preset
"
  (setf *poolplayer-presets-file*
        (namestring
         (or presets-file
             (let ((default-presets
                     (merge-pathnames
                      "packages/cl-poolplayer/presets/cl-poolplayer-default-presets.lisp"
                      (asdf:system-source-directory :clamps))))
               (uiop:run-program
                (format nil "/usr/bin/cp ~A /tmp/poolplayer-presets.lisp"
                        default-presets))
               "/tmp/poolplayer-presets.lisp"))))
  (load-poolplayer-presets)
  (setf *curr-poolplayer-preset-no* 0)
  (init-poolplayer-elisp-code
   (or elisp-file
       (namestring
        (merge-pathnames
         "sly-edit-poolplayer-presets.el"
         (asdf:system-source-directory :cl-poolplayer))))))

(defun get-preset-form (idx)
  (cl-poolplayer::preset-form (elt *poolplayer-presets* idx)))

(defun get-preset-string (idx)
  (with-output-to-string (out)
    (loop for (key value) on (get-preset-form idx) by #'cddr
          for start = "(" then #\NEWLINE
          do (format out "~a~s ~s" start key value))
    (format out ")")))

