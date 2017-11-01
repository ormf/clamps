;;; cm-utils.lisp
;;;
;;; Copyright (c) 2017 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package #:cm)

;;(defparameter *local-time* 0)

;;; extension for a more general process type than in cm

(defmacro rt-wait (time &optional (yield t))
  `(progn
     (cm:wait ,time)
;;     (incf *local-time* ,time)
     (cl-coroutine:yield ,yield)))

(defmacro rt-sprout (s-expr &key (at))
  `(sprout ,s-expr :at (or ,at (now))))

(defmacro rt-proc (&body body)
  (alexandria:with-gensyms (name)
    `(progn
       (cl-coroutine:defcoroutine ,name () ,@body)
       (cl-coroutine:make-coroutine ',name))))

(defmacro rt-sub (&rest rest)
  (alexandria:with-gensyms (fn)
    `(let ((,fn (eval ,@rest)))
       (loop
          while (funcall ,fn) 
          do (cl-coroutineyield t)))))

;;; channel-tuning utility

(defmacro make-mt-stream (symbol-name midi-out-stream chan-tuning)
  "Define, open and initialize a microtonal midistream. The name of
the stream and an already initialized midi-port-stream has to be
supplied and gets interned as a parameter."
  `(progn
     (defparameter ,symbol-name
       (new incudine-stream
         :name (string-trim '(#\*) (format nil "~a" ',symbol-name))
         :output ,midi-out-stream))
     (open-io (apply #'init-io ,symbol-name
                     `(:channel-tuning ,,chan-tuning)) :output ,midi-out-stream)
     (initialize-io ,symbol-name)
     (values ',symbol-name)))

(defun drunk-traverse (seq &key (weight 0.5))
  "shuffle an ordered list slightly by randomly swapping the positions
of neighboring elements."
  (cond ((null seq) nil)
        ((null (cdr seq)) seq)
        (:else (if (< (random 1.0) weight)
                   (cons (first seq) (drunk-traverse (rest seq) :weight 0.5))
                   (cons (second seq) (drunk-traverse
                                       (cons (first seq)
                                             (nthcdr 2 seq)) :weight 1))))))
