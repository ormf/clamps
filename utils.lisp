(in-package :of-incudine-dsps)

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(defparameter *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))
