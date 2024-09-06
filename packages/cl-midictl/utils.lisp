;;; 
;;; utils.lisp
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

(defun ccin (ccnum &optional (channel *global-midi-channel*))
  "Return the last received MIDI CC value of controller number /ccnum/
at MIDI channel /channel/. Setfable.

@Arguments
ccnum - Integer in the range [1..128] indicating the Controller Number.
channel - Integer in the range [1..16] indicating the MIDI channel.

@See-also
*midi-cc-state*
"
  (get-val (aref (aref *midi-cc-state* (1- channel)) (1- ccnum))))

(defsetf ccin (ccnum &optional (channel *global-midi-channel*)) (value)
  "Set the last received MIDI CC value of controller number <ccnum> at
MIDI channel <channel>."
  `(progn
     (set-val (aref (aref *midi-cc-state* ,(1- channel)) ,(1- ccnum)) ,value)
     ,value))

(defun get-ref (controller ref-idx)
  "return the ref-object of the midi-controller <controller> given the
<ref-idx> indexing into the cc-nums slot of the controller."
  (with-slots (cc-nums cc-state) controller
    (aref cc-state (aref cc-nums ref-idx))))

(defmacro toggle-slot (slot)
  `(set-val ,slot
            (if (zerop (get-val ,slot))
                127 0)))

(defun buchla-scale (curr old target &key (max 127))
  "Set the <target> fader by interpolating between 0 and <max>, using
the <curr> and <old> values of a source fader.

The function serves the purpose of avoiding jumps when working with
non motorized hardware faders: If the value of the software target
of the hardware fader has changed (e.g. by a preset or some program
logic) without the hardware fader being updated, moving the
hardware fader will not cause a jump in the target:

If the hardware fader moves up (> curr old), the remaining space
above the fader will interpolate the target software fader between
its current value and the maximum value, if it moves down (< curr
old), the software target will be interpolated between the current
value and 0 using the remaining space below the hardware fader.
"
  (float
   (cond
     ((= old target) curr)
     ((= curr old) target)
     ((< curr old)
      (* (- 1 (/ (- old curr) old)) target))
     (t (- max (* (- 1 (/ (- curr old) (- max old))) (- max target)))))
   1.0))

(defmacro with-gui-update-off ((instance) &body body)
  `(progn
     (setf (gui-update-off ,instance) t)
     (unwind-protect ,@body
       (setf (gui-update-off ,instance) nil))))
