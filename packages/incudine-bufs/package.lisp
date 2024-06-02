;;;; package.lisp

(in-package :cl-user)

(defpackage :incudine-bufs
  (:use #:cl #:incudine)
  (:export #:of-buffer-load #:ensure-buffer #:buffer-id #:get-buffer #:find-buffer
           #:add-buffer #:remove-buffer #:remove-all-buffers
           #:path-find-file #:get-sndfile-path))

