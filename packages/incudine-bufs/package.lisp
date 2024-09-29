;;;; package.lisp

(in-package :cl-user)

(defpackage :incudine-bufs
  (:use #:cl #:incudine)
  (:export #:clamps-buffer-load #:ensure-buffer #:buffer-id #:get-buffer-file
           #:find-buffer #:add-buffer #:list-buffers #:remove-buffer #:remove-all-buffers
           #:path-find-file #:get-sndfile-path))

