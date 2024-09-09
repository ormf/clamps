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
(defparameter *global-midi-channel* 1
  "Default MIDI channel for midi controllers or access functions like
<<ccin>>.

@See-also
ccin
midi-controller
")

(defparameter *midi-cc-state*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect (make-ref 0)))))
  "2-dimensional Array of 16x128 <<ref-object><ref-objects>> reflecting the last received
CC value of a MIDI CC message for all 128 CC numbers on all 16 MIDI
channels.

@See-also
ccin
*midi-cc-fns*
*midi-note-fns*
*midi-note-state*
")

(defparameter *midi-cc-fns*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect nil))))
  "2-dimensional Array of 16x128 lists containing functions to be called
on a received MIDI CC message individually for the 128 CC numbers on
all 16 MIDI channels with the CC value as argument.

@See-also
*midi-cc-state*
*midi-note-fns*
*midi-note-state*
")

(defparameter *midi-note-state*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect (make-ref 0)))))
  "2-dimensional Array of 16x128 <<ref-object><ref-objects>> reflecting the last received
velocity of a MIDI note on message for all 128 keynums on all 16 MIDI
channels.

@See-also
*midi-cc-fns*
*midi-cc-state*
*midi-note-fns*
")

(defparameter *midi-note-fns*
  (make-array 16 :initial-contents
              (loop repeat 16
                    collect (make-array 128 :initial-contents (loop repeat 128 collect nil))))
  "2-dimensional Array of 16x128 lists containing functions to be called
on a received MIDI note on message individually for all 128 keynums on
all 16 MIDI channels with the velocity as argument.

@See-also
*midi-cc-fns*
*midi-cc-state*
*midi-note-state*
")
