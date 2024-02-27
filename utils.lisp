(in-package :of-incudine-dsps)

#+sbcl
(sb-ext:defglobal *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(sb-ext:defglobal *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))

#|
#-sbcl
(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(defparameter *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))
|#

(defun restore-envs ()
  (setq *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
  (setq *hanning1024* (make-buffer 1024 :fill-function (gen:hanning))))
