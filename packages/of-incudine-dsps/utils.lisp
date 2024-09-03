(in-package :of-incudine-dsps)

#+sbcl
(sb-ext:defglobal *env1* (make-envelope '(0 1 1 0) '(0 .9 .1))
    "Incudine three point ASR envelope with attack time 0 and release time
0.1 of the total duration.")
(sb-ext:defglobal *hanning1024* (make-buffer 1024 :fill-function (gen:hanning))
    "Incudine buffer of length 1024 containing a Hanning window.")
(sb-ext:defglobal *sine1024* (make-buffer 1024 :fill-function (gen:partials '(1)))
    "Incudine buffer of length 1024 containing one period of a sine wave.")

#|
#-sbcl
(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(defparameter *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))
|#

(defun restore-envs ()
  (setq *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
  (setq *hanning1024* (make-buffer 1024 :fill-function (gen:hanning))))
