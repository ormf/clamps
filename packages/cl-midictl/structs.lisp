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

@See-also
midi-port-id
midi-port-input
midi-port-output
midi-port-cc-state
midi-port-note-state
midi-port-pitch-bend-state
midi-port-after-touch-state
midi-port-cc-fns
midi-port-note-fns
midi-port-pitch-bend-fns
midi-port-after-touch-fns
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


