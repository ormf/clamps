;;; 
;;; utils.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :of-incudine-dsps)

(defmacro arglist-ansure-samples (proplist args)
  "Ensure that all properties from proplist existing in args are of type
sample."
  `(progn ,@(mapcar (lambda (prop)
                      `(when (getf ,args ,prop)
                         (setf (getf ,args ,prop) (float (getf ,args ,prop) 1.0d0))))
                    proplist)))

#+sbcl
(sb-ext:defglobal *env1* (make-envelope '(0 1 1 0) '(0 .9 .1))
    "Incudine three point ASR envelope with attack time 0 and release time
0.1 of the total duration.

@Example-nosrc
#+BEGIN_SRC lisp
(plot *env1*)
;; => nil
#+END_SRC
#+attr_html: :width 50%
#+CAPTION: *​env1​* envelope
[[./img/env1-plot.svg]]
")

(sb-ext:defglobal *hanning1024* (make-buffer 1024 :fill-function (gen:hanning))
    "Incudine buffer of length 1024 containing a Hanning window.

@Example-nosrc
#+BEGIN_SRC lisp
(plot *hanning1024*)
;; => #<incudine:buffer :FRAMES 1024 :CHANNELS 1 :SR 44100.0>
#+END_SRC

#+attr_html: :width 50%
#+CAPTION: *​hanning1024​* buffer
[[./img/hanning-plot.svg]]
@See-also
*sine1024*
")

(sb-ext:defglobal *sine1024* (make-buffer 1024 :fill-function (gen:partials '(1)))
    "Incudine buffer of length 1024 containing one period of a sine wave.

@Example-nosrc
#+BEGIN_SRC lisp
(plot *sine1024*)
;; => #<incudine:buffer :FRAMES 1024 :CHANNELS 1 :SR 44100.0>
#+END_SRC

#+attr_html: :width 50%
#+CAPTION: *​sine1024​* buffer
[[./img/sine-plot.svg]]

@See-also
*hanning1024*
")

#|
#-sbcl
(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(defparameter *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))
|#

(defun restore-envs ()
  (setq *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
  (setq *hanning1024* (make-buffer 1024 :fill-function (gen:hanning))))
