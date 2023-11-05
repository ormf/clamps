;;; 
;;; osc.lisp
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

(defparameter *local-host* "127.0.0.1")

;;; this has to be defined for every registered host:
;;; (defparameter *oscout* (incudine.osc:open :port 4711 :host *remote-host* :direction :output :protocol :udp))
(defparameter *osc-midi-in* nil)
(defparameter *midictl-osc-responders* (make-hash-table)) ;;; a hashtable with the handles of all registered midi responders

(defparameter *midictl-osc-remote-connections* nil)

(defun osc-midi-add-remote-connection (host local-midi-in &key (port 4710))
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete entry *midictl-osc-remote-connections*))))
  (let ((connection (incudine.osc:open :port port :host host :direction :output :protocol :udp)))
    (push connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/msg" "s" "connected")
    (sleep 1)
    (dolist (obj (gethash local-midi-in *midi-controllers*)) (update-state obj))))

(defun osc-midi-remove-remote-connection (host)
  (let ((entry (first (member host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal))))
    (when entry
      (incudine.osc:close entry)
      (setf *midictl-osc-remote-connections*
            (delete host *midictl-osc-remote-connections* :key #'incudine.osc:host :test #'string-equal)))))

(defun osc-midi-remove-all-remote-connections ()
  (dolist (conn *midictl-osc-remote-connections*)
    (incudine.osc:close conn))
  (setf *midictl-osc-remote-connections* nil))

;;; osc responder:

(defun start-osc-midi-receive (local-midi-in &key (port 4711))
  "start osc on localhost:port and its receivers."
  (when *osc-midi-in* (incudine.osc:close *osc-midi-in*))
  (maphash (lambda (key val) key (incudine:remove-responder val)) *midictl-osc-responders*)
  (setf *osc-midi-in* (incudine.osc:open :host "127.0.0.1" :port port :direction :input :protocol :udp))
  (setf (gethash :osc-midi-register *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/osc-midi-register" "sf"
         (lambda (host port)
           (let ((port (round port)))
             (incudine.util:msg :info "osc-midi-register: ~a ~a" host port)
             (osc-midi-add-remote-connection host local-midi-in :port port)))))
  (setf (gethash :midiin *midictl-osc-responders*)
        (incudine::make-osc-responder
         *osc-midi-in* "/midiin" "fff"
         (lambda (st d1 d2)
           (let ((st (round st))
                 (d1 (round d1))
                 (d2 (round d2)))
             (incudine::msg :info "~&~S ~a ~a ~a"
                            (status->opcode st) d1 d2 (status->channel st))
             (let ((chan (status->channel st))
                   (opcode (status->opcode st)))
               (generic-midi-handler opcode d1 d2 chan)
               (dolist (controller (gethash local-midi-in *midi-controllers*))
                 (if (= chan (chan controller))
                     (handle-midi-in controller opcode d1 d2))))))))
  (recv-start *osc-midi-in*))

(defun stop-osc-midi-receive (&optional local-midi-in)
  (declare (ignore local-midi-in))
   (recv-stop *osc-midi-in*))

(defun osc-midi-broadcast (st d1 d2)
  (dolist (connection *midictl-osc-remote-connections*)
    (incudine.osc:message connection "/midiout" "iii" st d1 d2)))

(defun osc-midi-write-short (stream st d1 d2)
  (jackmidi:write-short stream (jackmidi:message st d1 d2) 3)
  (osc-midi-broadcast st d1 d2))
