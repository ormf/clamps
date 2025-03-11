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
  (get-val (aref (aref *midi-cc-state* (1- channel)) ccnum)))

(defsetf ccin (ccnum &optional (channel *global-midi-channel*)) (value)
  "Set the last received MIDI CC value of controller number <ccnum> at
MIDI channel <channel>.

@Arguments
ccnum - Integer in the range [1..128] indicating the Controller Number.
channel - Integer in the range [1..16] indicating the MIDI channel.
"
  `(progn
     (set-val (aref (aref *midi-cc-state* ,(1- channel)) ,ccnum) ,value)
     ,value))

(defun get-ref (controller ref-idx)
  "Return the ref-object of the midi-controller /controller/ given the
/ref-idx/ indexing into the cc-nums slot of the controller.

@Arguments
controller - Instance of type midi-controller.
ref-idx - Non Negative Integer denoting the index of the cc-nums array of the controller."
  (with-slots (cc-nums cc-state) controller
    (aref cc-state (aref cc-nums ref-idx))))

(defmacro toggle-slot (slot)
  `(set-val ,slot
            (if (zerop (get-val ,slot))
                127 0)))

(defun buchla-scale (curr old target &key (max 127))
  "Set /target/ fader by interpolating between 0 and /max/, using
the /curr/ and /old/ values of a source fader.

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
  "Evaluate code of /body/ in the context of the gui-update-off slot of
/instance/ set to t. After the body has evaluated, the gui-update-off
slot is set to its previous value before entering body.

@Arguments
instance - Instance of type [[midi-controller]].
body - Zero or more lisp forms.
"
  `(let ((tmp (gui-update-off ,instance))
         (setf (gui-update-off ,instance) t))
     (unwind-protect ,@body
       (setf (gui-update-off ,instance) tmp))))

(defun toggle-ref-watch (ref &optional (modulo 2))
  "Install a function to cycle the value of /ref/ between 0 and (1-
/modulo/ on trigger. Returns its uninstall function."
  (let ((fn (toggle-ref-fn ref modulo)))
    (pushnew fn (trigger-fns ref))
    (lambda () (setf (trigger-fns ref) (remove fn (trigger-fns ref))))))

(defun make-led-pulsar (ccnum chan midi-output &key (freq 2) (pulse-width 0.5))
  "Return an \"instance\" (closure) of a pulse generator flashing a LED
on an external MIDI Harware device by sending the values 0/127 using
/ccnum/, /chan/ and /midi-output/.

Funcalling the returned instance with the :start argumnet will start
the pulse, funcalling it with the :stop argument will stop flashing.

@Example

(defvar *my-pulsar* (make-led-pulsar 32 1 (cm:ensure-jackmidi:*midi-out1*)))

(funcall *my-pulsar*:start)

(funcall *my-pulsar*:stop)

"
  (let (node-id
        running)
    (lambda (cmd)
      (case cmd
        (:start (unless running
                  (incudine::pulse-midi-led chan ccnum :midi-out midi-output :freq freq :pulse-width pulse-width :tail 100
                                                       :action (lambda (id) (setf node-id id)))
                  (setf running t)))
        (:stop (free node-id)
         (when running
           (setf running nil)
           (cl-midictl:osc-midi-write-short
            midi-output
            (+ (1- chan) 176) ccnum 0)))))))

(in-package :incudine)

(define-vug pulse-0-1 (freq width)
  "Pulse wave oscillator with frequency FREQ, amplitude AMP and WIDTH
between 0 and 1."
  (:defaults 4 .5)
  (if (< (phasor freq 0) width) 1 0))

(dsp! pulse-midi-led ((chan channel-number) (cc-num channel-number) freq pulse-width (midi-out jackmidi:output-stream))
  "Pulse an LED on an external MIDI Hardware controller with freq and
pulse-width by sending  0/127 to /ccnum/ on /chan/ using /midi-out/ ."
  (:defaults 0 0 0.5 0.5 (incudine:incudine-missing-arg "MIDI-OUTPUT"))
  (with ((state 0))
    (foreach-frame
      (let ((new-state (round (pulse-0-1 freq pulse-width))))
        (when (/= new-state state)
            (cl-midictl:osc-midi-write-short
             midi-out
             (+ (1- chan) 176) cc-num (if (zerop state) 127 0))
            (setf state new-state))))))
