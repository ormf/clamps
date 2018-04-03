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

(defun testmidi () (output (new midi)))

(defun jbmf ()
  (output (new midi-program-change :program 24))
  (sprout
   (let ((tscale 0.3))
     (process
       for event in
       '((1 43 3) (1 67 1) (1 69 1) (0 55 3) (0 71 1) (0 67 2) (1 62 3) (1 74 1)
         (0 72 1) (1 66 1) (0 67 2) (0 72 1) (0 52 3) (1 64 3) (1 76 1) (0 69 1)
         (1 74 1) (0 71 2) (0 47 3) (0 74 1) (1 67 3) (1 79 1) (0 78 1) (1 69 1)
         (0 79 1) (0 71 2) (0 64 3) (1 52 3) (1 74 1) (0 67 1) (1 71 1) (0 67 1)
         (0 64 2) (0 40 3) (1 59 3) (1 69 1) (0 62 1) (1 71 1) (0 64 2) (0 72 1)
         (0 45 3) (1 57 3) (1 74 1) (0 76 1) (1 66 1) (0 62 2) (0 47 3) (0 67 3)
         (1 74 1) (1 72 1) (0 62 1) (1 71 1) (0 64 3) (0 48 3) (0 69 1) (1 60 2)
         (1 71 1) (0 59 1) (1 67 1) (0 57 3) (0 60 2) (0 66 1) (1 50 3) (1 67 1)
         (0 62 1) (1 69 1) (0 69 2) (0 54 3) (0 57 3) (1 62 1) (1 66 1) (0 69 1)
         (1 67 1) (0 72 1) (0 69 2) (0 50 3) (1 66 2) (1 71 1) (0 66 1) (1 69 1)
         (0 71 1) (0 55 3) (0 67 1) (1 62 2) (1 67 1) (0 69 1) (1 66 1) (0 71 1)
         (0 67 2) (1 55 3) (1 74 1) (0 72 1) (1 66 1) (0 72 1) (0 64 3) (0 48 3)
         (1 67 2) (1 76 1) (0 74 1) (1 69 1) (0 71 2) (0 74 1) (0 67 3) (1 47 3)
         (1 79 1) (0 78 1) (1 69 1) (0 52 3) (0 79 1) (0 71 2) (1 64 3) (1 74 1)
         (0 71 1) (1 67 1) (0 64 2) (0 67 1) (0 50 3) (1 59 3) (1 69 1) (0 71 1)
         (1 67 1) (0 69 3) (0 60 2) (0 64 1) (1 48 3) (1 74 1) (0 72 1) (1 66 1)
         (0 71 1) (0 67 2) (0 49 3) (1 64 1.95) (1 69 1) (0 67 1) (1 64 1) (0 50 3)
         (0 62 1) (1 57 2) (1 67 1) (0 60 1) (1 66 1) (0 43 3) (0 62 2) (0 67 1)
         (1 59 3) (1 71 1) (0 74 1) (1 67 1) (0 79 1) (1 71 1.95) (1 74 1) (1 71 1)
         (1 67 3) )
       do (output (new midi :time (now)
                       :keynum (second event)
                       :amplitude 0.3
                       :duration (* tscale 1 (third event))))
       wait (* tscale (first event))
       finally (output (new midi-program-change :program 0))))))

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
                                             (nthcdr 2 seq)) :weight 0.5))))))


;;; (drunk-traverse '(1 2 3 4 5 6 7))

