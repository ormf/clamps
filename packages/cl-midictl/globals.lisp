;;; 
;;; globals.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-midictl)

(defparameter *midi-in1* nil)
(defparameter *midi-out1* nil)
(defparameter *global-midi-channel* 0)

(defparameter *midi-cc-state*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect (make-ref 0))))))

(defparameter *midi-cc-fns*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect nil)))))

(defparameter *midi-note-state*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect (make-ref 0))))))

(defparameter *midi-note-fns*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect nil)))))
