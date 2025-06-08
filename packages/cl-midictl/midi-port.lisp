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

(defparameter *midi-ports* nil
  "private proplist containing the midi-port ids and structs as key/value pairs.")

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

(defun list-midi-ports (&key (sort t))
  "Return a list of the ids of all registered Midi Ports. If /sort/ is
non-nil, return a sorted list of the ids, otherwise return the ids in
the reverse order of their instantiation.

@Arguments
:sort - Boolean denoting sort order of the ids. Defaults to t.

@See-also
close-midi-port
find-midi-port
open-midi-port
"
  (let ((port-ids (loop for (id port) on *midi-ports* by #'cddr
                     collect id)))
    (if sort
        (sort port-ids
              (lambda (x y) (string< (symbol-name x) (symbol-name y))))
        port-ids)))

(defun open-midi-port (id &key (start-receiver t))
  "Register a new midi-port struct, open its midi input and output,
define and start its default responders if /start-receiver/ is t.
/id/ is used for the internal registry and the ports in jack will be
named <id>-in and <id>-out. Returns the midi-port struct.

@Arguments
id - Keyword or Symbol denoting the id of the midi port.
:start-receiver - Boolean indicating whether to start the receiver.

@See-also
close-midi-port
find-midi-port
list-midi-ports
"
  (if (find-midi-port id) (error "midi-port ~S already open" id)
      (progn
        (incudine.util:msg :warn "opening midi-port ~S" id)
        (let ((new-midi-port
                (let ((in-name (format nil "~a-in" id))
                      (out-name (format nil "~a-out" id)))
                  (make-midi-port :id id
                                  :in (jackmidi:open :port-name in-name)
                                  :out (jackmidi:open :direction :output :port-name out-name)))))
          (setf *midi-ports* (list* id new-midi-port *midi-ports*))
          (when start-receiver
            (loop repeat 20 until (midi-port-in new-midi-port)
                  do (progn
                       (incudine.util:msg :warn "waiting for midi-in of ~s" cl-midictl::id)
                       (sleep 0.1)))
            (loop repeat 20 until (midi-port-out new-midi-port)
                  do (progn
                       (incudine.util:msg :warn "waiting for midi-out of ~s" cl-midictl::id)
                       (sleep 0.1)))
            (start-midi-receive new-midi-port))
          new-midi-port))))

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

;;; util macros

(defun midi-port-input (id)
  "Return the jackmidi input stream of midi-port /id/.

@Arguments
id - Keyword denoting the id of a midi-port or a midi-port struct."
  (typecase id
    (keyword (midi-port-in (find-midi-port id)))
    (midi-port (midi-port-in id))
    (otherwise (error "no midi-port with id ~s found." id))))

(defun midi-port-output (id)
  "Return the jackmidi output stream of midi-port /id/.

@Arguments
id - Keyword denoting the id of a midi-port or a midi-port struct."
  (typecase id
    (keyword (midi-port-out (find-midi-port id)))
    (midi-port (midi-port-out id))
    (otherwise (error "no midi-port with id ~s found." id))))
