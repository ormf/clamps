;;; 
;;; nanoctl.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defclass nanoctl (midi-controller) ())

(defmethod handle-midi-in ((instance nanoctl) opcode d1 d2)
  (with-slots (cc-fns cc-map cc-state note-fn last-note-on midi-output chan) instance
    (case opcode
      (:cc (progn
             (case d1
               (42 (if (and (> d2 0) (> (aref cc-state (aref cc-map 41)) 0)) ;;; stop
                       (progn (setf (aref cc-state (aref cc-map 41)) 0)
                              (if midi-output (funcall (ctl-out midi-output 41 0 chan))))))
               (41 (if (and (> d2 0) (zerop (aref cc-state (aref cc-map 41))))
                       (progn
                         (setf (aref cc-state (aref cc-map 41)) 127)
                         (if midi-output (funcall (ctl-out midi-output 41 127 chan))))))
               (otherwise (setf (aref cc-state (aref cc-map d1)) d2)))
             (funcall (aref cc-fns d1) d2)))
      (:note-on (progn
                  (funcall (note-fn instance) d1 d2)
                  (setf last-note-on d1)))
      (:note-off (funcall (note-fn instance) d1 0)))))

;;; (make-instance 'nanoctl)

;;; (defparameter cm:*midi-in1* (jackmidi:open ))


