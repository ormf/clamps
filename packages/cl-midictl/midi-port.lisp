;;; 
;;; midi-port.lisp
;;;
;;; Infrastructure for midi-in and midi-out.
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

(defparameter *midi-ports* nil)

(defun find-midi-port (id)
  "Return a registered midi port denoted by /id/.

@Arguments
id - Keyword or Symbol denoting the id of the midi port.

@See-also
close-midi-port
list-midi-ports
open-midi-port
"
  (getf *midi-ports* id))

(defun list-midi-ports ()
  "Return a property list with all Midi Ports with their id as key and
their midi-port structs as value.

@See-also
close-midi-port
find-midi-port
open-midi-port
"
  *midi-ports*)

(defun open-midi-port (id)
  "Register a new midi port struct and return the struct.

@Arguments
id - Keyword or Symbol denoting the id of the midi port.

@See-also
close-midi-port
find-midi-port
list-midi-ports
"
  (if (find-midi-port id) (error "midi-port ~S already open" id)
      (let ((new-midi-port
              (let ((in-name (format nil "~a-in" id))
                    (out-name (format nil "~a-out" id)))
                (make-midi-port :id id
                                :in (jackmidi:open :port-name in-name)
                                :out (jackmidi:open :direction :output :port-name out-name)))))
        (setf *midi-ports* (list* id new-midi-port *midi-ports*))
        new-midi-port)))

(defun close-midi-port (id)
  "Remove a midi port denoted by /id/. Return t if successful.

@Arguments
id - Keyword or Symbol denoting the id of the midi port.

@See-also
find-midi-port
list-midi-ports
open-midi-port
"
  (let ((midi-port (find-midi-port id)))
    (unless midi-port  (error "midi-port ~S not found" id))
    (jackmidi:close (midi-port-in midi-port))
    (jackmidi:close (midi-port-out midi-port))
    (remf *midi-ports* id)))

;;; *midi-ports*

;;; '(#xF0 #x00 #x00 #x66 #x10 #x12 #x00 #x48 #x65 #x6C #x6C #x6F #xF7)

 ; => (240 0 0 102 16 18 0 72 101 108 108 111 247)
#|
(open-midi-port :midi-1)
(open-midi-port :midi-2)
(open-midi-port :midi-3)

(find-midi-port :midi-1)

(incudine:midiout-sysex)

(progn
  (close-midi-port :midi-1)
  (close-midi-port :midi-2)
  (close-midi-port :midi-3))

|#
