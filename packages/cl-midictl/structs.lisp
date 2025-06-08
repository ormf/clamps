;;; 
;;; structs.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defstruct midi-port
  "Structure of a bidirectional midi port, containing slots for
cc-state, note-state, pitch-bend-state, after-touch-state, cc-fns,
note-fns, pitch-bend-fns and after-touch-fns.

@Slots
midi-port-id - Keyword or Symbol denoting the id of the midi-port.
midi-port-input - Jackmidi:input-stream
midi-port-output - Jackmidi:output-stream
midi-port-cc-state - Array of 128x16 <<ref-object><ref-objects>> storing the last received cc value.
midi-port-note-state - Array of 128x16 <<ref-object><ref-objects>> storing the last received note-on velocity.
midi-port-pitch-bend-state - Array of 16 <<ref-object><ref-objects>> storing the last received pitch-bend value.
midi-port-after-touch-state - Array of 16 <<ref-object><ref-objects>> storing the last received after-touch value.
midi-port-cc-fns - Array of 128x16 lists containing functions to call on receiving a cc value.
midi-port-note-fns - Array of 128x16 lists containing functions to call on receiving a note-on with positive velocity.
midi-port-pitch-bend-fns - Array of 16 lists containing functions to call on receiving a pitch-bend value.
midi-port-after-touch-fns - Array of 16 lists containing functions to call on receiving a after-touch value.

@See-also
midi-port
"
  (id)
  (in)
  (out)
  (cc-state (apply #'vector
                   (loop repeat 16
                         collect (apply #'vector (loop repeat 128 collect (make-ref 0))))))
  (note-state (apply #'vector
                   (loop repeat 16
                         collect (apply #'vector (loop repeat 128 collect (make-ref 0))))))
  (pitch-bend-state (apply #'vector (loop repeat 16 collect (make-ref 0))))
  (after-touch-state (apply #'vector (loop repeat 16 collect (make-ref 0))))
  (cc-fns (apply #'vector
                   (loop repeat 16
                         collect (apply #'vector (loop repeat 128 collect nil)))))
  (note-fns (apply #'vector
                   (loop repeat 16
                         collect (apply #'vector (loop repeat 128 collect nil)))))
  (pitch-bend-fns (apply #'vector (loop repeat 16 collect nil)))
  (after-touch-fns (apply #'vector (loop repeat 16 collect nil)))

  )


