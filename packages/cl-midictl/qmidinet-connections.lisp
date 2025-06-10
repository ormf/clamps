;;; 
;;; qmidinet-connections.lisp
;;;
;;; Handling Asparion switch between MIO Console on Mac and Linux
;;; using qmidinet/ipmidi
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

(defparameter *current-connection* :linux)

(defparameter *connection-tables*
  '(:persistent
    ((:qmidinet-capture-0 :incudine-midi-3-in)
     (:qmidinet-capture-1 :incudine-midi-4-in)
     (:incudine-midi-3-out :d700-playback-0)
     (:incudine-midi-4-out :d700-playback-1))
    :linux
    ((:d700-capture-0 :incudine-midi-1-in)
     (:d700-capture-1 :incudine-midi-2-in)
     (:incudine-midi-1-out :d700-playback-0)
     (:incudine-midi-2-out :d700-playback-1))
    :mac
    ((:d700-capture-0 :qmidinet-playback-0)
     (:d700-capture-1 :qmidinet-playback-1)
     (:qmidinet-capture-0 :d700-playback-0)
     (:qmidinet-capture-1 :d700-playback-1)
     (:d700-capture-0 :incudine-midi-3-in)
     (:d700-capture-1 :incudine-midi-4-in)
)))

(defun get-jack-port-names ()
  "return all port names printed by a call to jack_lsp to stdout in a
list."
  (with-input-from-string (in
                           (with-output-to-string (out)
                             (uiop:run-program "/usr/bin/jack_lsp" :output out)))
    (loop
      for line = (read-line in nil nil)
      while line
      collect line)))

(defparameter *jack-regex-assoc*
  '((:incudine-midi-1-in "incudine:midi-1-in")
    (:incudine-midi-2-in "incudine:midi-2-in")
    (:incudine-midi-3-in "incudine:midi-3-in")
    (:incudine-midi-4-in "incudine:midi-4-in")
    (:incudine-midi-1-out "incudine:midi-1-out")
    (:incudine-midi-2-out "incudine:midi-2-out")
    (:incudine-midi-3-out "incudine:midi-3-out")
    (:incudine-midi-4-out "incudine:midi-4-out")
    (:d700-capture-0 "a2j:D 700.+capture.+D 700 MIDI 1")
    (:d700-capture-1 "a2j:D 700.+capture.+D 700 MIDI 2")
    (:d700-playback-0 "a2j:D 700.+playback.+D 700 MIDI 1")
    (:d700-playback-1 "a2j:D 700.+playback.+D 700 MIDI 2")
    (:qmidinet-capture-0 "a2j:QmidiNet.+capture.+port 0")
    (:qmidinet-capture-1 "a2j:QmidiNet.+capture.+port 1")
    (:qmidinet-playback-0 "a2j:QmidiNet.+playback.+port 0")
    (:qmidinet-playback-1 "a2j:QmidiNet.+playback.+port 1")))

(defparameter *jack-strings-to-keep*
  '("a2j:D 700" "incudine:midi-" "a2j:QmidiNet"))

(defun matching (a b)
  "test if beginning of a matches b."
  (let ((len (length b)))
    (and (>= (length a) len)
         (string= (subseq a 0 len) b))))

(defun get-jack-port-hash (jack-names)
  "Match the beginning of all jack-names against *jack-strings-to-keep*
and add an association between the keyword in *jack-regex-assoc* and
the jack-name to a hash table if the jack-name matches the regex of
that keyword. A new hash table is initialized at function
startup. Return the hash table."
  (let ((hash (make-hash-table)))
    (loop
      for name in jack-names
      if (some (lambda (str) (matching name str)) *jack-strings-to-keep*)
        do (some (lambda (assoc) (if (cl-ppcre:scan (second assoc) name) (setf (gethash (first assoc) hash) name))) *jack-regex-assoc*))
    hash))

(defparameter *jack-port-hash* (get-jack-port-hash (get-jack-port-names)))

(defun jack-disconnect (src dest)
  (let ((command (format nil "jack_disconnect \"~a\" \"~a\"" (gethash src *jack-port-hash*) (gethash dest *jack-port-hash*))))
;;;    (incudine.util:msg :warn command)
    (uiop:run-program command
                      :ignore-error-status t)))

(defun jack-connect (src dest)
  (let ((command (format nil "jack_connect \"~a\" \"~a\"" (gethash src *jack-port-hash*) (gethash dest *jack-port-hash*))))
;;;    (incudine.util:msg :warn command)
    (uiop:run-program  command
     :ignore-error-status t)))

(defun jack-init-connections ()
  (setf *jack-port-hash* (get-jack-port-hash (get-jack-port-names)))
  (dolist (connection
           (append (getf *connection-tables* :mac)
                   (getf *connection-tables* :linux)))
    (apply #'jack-disconnect connection))
  (dolist (connection (getf *connection-tables* :persistent))
    (apply #'jack-connect connection)))

(defun jack-linux ()
  (dolist (connection
           (append (getf *connection-tables* :mac)))
    (apply #'jack-disconnect connection))
  (dolist (connection (getf *connection-tables* :linux))
    (apply #'jack-connect connection)))

(defun jack-mac ()
  (dolist (connection
           (append (getf *connection-tables* :linux)))
    (apply #'jack-disconnect connection))
  (dolist (connection (getf *connection-tables* :mac))
    (apply #'jack-connect connection)))

(defun to-linux ()
  (let ((controller (find-controller :asparion)))
    (jack-linux)
    (set-val (star-button controller) 1)
    (update-hw-state controller)
    (setf *current-connection* :linux)))

(defun to-mac ()
  (let* ((controller (find-controller :asparion-mac)))
    (jack-mac)
    (set-val (star-button controller) 0)
    (update-hw-state controller)
    (setf *current-connection* :mac)))

(defun toggle-connection ()
  (case *current-connection*
    (:linux (to-mac))
    (:mac (to-linux))))

(defun asparion-star-press-action ()
  (toggle-connection))

#|
;; After midi ports for qmidinet have been established on the linux
;; machine and the Mac and the Asparion has been connected to the
;; linux machine, execute the following code:

(progn
  (remove-midi-controller :asparion)
  (remove-midi-controller :asparion-mac)
  (add-midi-controller 'asparion :asparion :midi-ports '(:midi-1 :midi-2))
  (add-midi-controller 'asparion :asparion-mac
                       :midi-ports '(:midi-3 :midi-4)
                       :echo nil
                       :rotary-ring nil :rotary-labels nil :fader-labels nil)
  (jack-init-connections)
  (jack-linux))


;;; Then open MIO program and switch layer (press "*" button on the
;;; asparion) to test.
|#
