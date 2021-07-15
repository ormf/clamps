;;; 
;;; load-sounds.lisp
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

(defun load-sounds (dir)
  "load all sounds in dir into an freshly allocated array of the size
of the number of files and return the array."
  (loop
    with snds = (directory dir)
    with buffers = (make-array (length snds)
                               :element-type 'incudine::buffer
                               :initial-element (incudine:make-buffer 1))
    for idx from 0
    for filename in snds
    do (setf (aref buffers idx) (incudine:buffer-load filename))
    finally (return buffers)))

;;; (cl-plot:plot (aref *buffers* 2))

(defun gen-poolplayer-buf-idxs (bufs)
  (loop
    with idx-hash = (make-hash-table)
    for idx from 0
    for buf across bufs
    do (setf (gethash buf idx-hash) idx)
    finally (return idx-hash)))

(defun load-poolplayer-sounds (dir sound-type-dirs)
  (setf *buffers* #())
  (loop for (type subdir) on sound-type-dirs by #'cddr
        with idx = 0
        do (progn
             (vector-extend *buffers* (load-sounds (format nil "~a/~a/*.wav" dir subdir)))
             (let ((curr-idx (length *buffers*)))
               (setf (gethash type *snd-type-hash*)
                     (loop for n from idx below curr-idx collect n))
               (setf idx curr-idx))))
  (setf *buffer-idxs* (gen-poolplayer-buf-idxs *buffers*)))

(defun collect-pool (&rest keys)
  (coerce
   (loop
     for key in keys
     for idxs = (gethash key *snd-type-hash*)
     append (loop for idx in idxs collect (aref *buffers* idx) ))
   'vector))

(defun buf-idx (buffer)
  (gethash buffer *buffer-idxs*))

