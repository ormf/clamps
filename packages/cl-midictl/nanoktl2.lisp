;;; 
;;; nanoktl2.lisp
;;;
;;; generic midi-controller for the nanokontrol2. The Controller
;;; should be connected to midi input and output to take full
;;; advantage of the code.
;;;
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

(defclass nanoktl2-midi (midi-controller)
  ((nk2-faders :accessor nk2-faders)
   (nk2-fader-update-fns :accessor nk2-fader-update-fns :initform (coerce (loop repeat 16 collect nil) 'vector))
   (nk2-fader-modes :accessor nk2-fader-modes :initform (coerce (loop repeat 16 collect :scale) 'vector))
   (nk2-fader-last-cc :accessor nk2-fader-last-cc :initform (coerce (loop repeat 16 collect 0) 'vector))
   (button-labels :initarg :button-labels :accessor button-labels
                  :initform (coerce (loop for i below 24 collect (make-ref (format nil "~a" (elt '("s" "m" "r") (floor i 8))))) 'vector)
                  :documentation "labels of preset buttons in gui (if present)")
   (hide-fader :accessor hide-fader :initform nil)
   (s-buttons :accessor s-buttons)
   (m-buttons :accessor m-buttons)
   (r-buttons :accessor r-buttons)
   (track-left :accessor track-left)
   (track-right :accessor track-right)
   (cycle :accessor nk-cycle)
   (set-marker :accessor set-marker)
   (marker-left :accessor marker-left)
   (marker-right :accessor marker-right)
   (tr-rewind :accessor tr-rewind)
   (tr-ffwd :accessor tr-ffwd)
   (tr-stop :accessor tr-stop)
   (tr-play :accessor tr-play)
   (tr-rec :accessor tr-rec)
   (cc-nums :accessor cc-nums))
  (:documentation "Class for a Nanoktl2 midi controller.

nanoktl2-midi implements the following slots with accessor methods of
the same name and initargs being the keywords of the slot symbol:

=cc-nums= -- Array of 128 elements containing the CC numbers of all
buttons and faders of the NanoKontrol2.

=nk2-faders= -- Array of 16 elements containing the
<<ref-object><ref-objects>> of the 8 knobs and the 8 faders of the
NanoKontrol2.

=nk2-fader-update-fns=

=nk2-fader-modes= Array of 16 elements containing the mode of the
fader when the hardware fader is out of sync with the program state of
the fader. Currently implemented are:

- /:scale/ Scale fader values when moving the hardware fader.
- /:jump/ Jump to the value when moving the hardware fader.

=nk2-fader-last-cc= Storage of the last CC value of the 16 faders.

=s-buttons= Array of 8 <<bang-object><ref-objects>> containing the state of the 8 S buttons with a value of 0 or 1.

=m-buttons= Array of 8 <<bang-object><ref-objects>> containing the state of the 8 M buttons with a value of 0 or 1.

=r-buttons= Array of 8 <<bang-object><ref-objects>> containing the state of the 8 R buttons with a value of 0 or 1.

=track-left= <<bang-object>> of the track left button.

=track-right= <<bang-object>> of the track right button.

=cycle= <<bang-object>> of the cycle button.

=set-marker= <<bang-object>> of the set marker button.

=marker-left= <<bang-object>> of the marker left button.

=marker-right= <<bang-object>> of the marker right button.

=tr-rewind= <<bang-object>> of the rewind transport button.

=tr-ffwd= <<bang-object>> of the fast forward transport button.

=tr-stop= <<bang-object>> of the stop transport button.

=tr-play= <<bang-object>> of the play transport button.

=tr-record= <<bang-object>> of the record transport button.


@See-also
midi-controller"))

(export '(nanoktl2-midi
          nk2-faders nk2-fader-update-fns nk2-fader-modes nk2-fader-last-cc hide-fader
          s-buttons m-buttons track-left track-right nk-cycle set-marker marker-left
          marker-right tr-rewind tr-ffwd tr-stop tr-play tr-rec cc-nums)
        'cl-midictl)

#|

cc-map is a lookup table mapping the cc nums of the nanokontrol2 to
the idx of the cc-state array slots. It is implemented as an array of
128 values. Undefined cc nums are set to nil.

The cc-map below is according to the factory settings of a
nanokontrol2.

00-08:     cc-nums of faders
08-15:     cc-nums of knobs
16-23:     cc-nums of s-buttons
24-31:     cc-nums of m-buttons
32-39:     cc-nums of r-buttons

40-41:     cc-nums of left and right button
50-53:     cc-nums of cycle, marker-set, marker-left and marker-right buttons
54-58:     cc-nums of transport buttons (left to right).

All buttons of the nanoktl2 should be put into momentary mode. In the
midi-controller instance, all buttons are realized as
bang-objects. Their value (0 or 1) is reflected in the respective LED
of the Hardware Controller.

The main idea behind this is to seperate the button-press action from
the button-state. The button state (on/off) is reflected in the value
of the bang-object, the button-press action is executed by triggering
the bang-object with the trigger function, which is the default
method, implemented in handle-midi-in. By setting the trigger-fn of a
bang-object to toggle-ref-fn, the momentary buttons can be easily
transformed into toggles.

|#

(defmethod initialize-instance :after ((obj nanoktl2-midi) &rest args)
  (with-slots (cc-map cc-nums cc-state chan nk2-faders s-buttons m-buttons r-buttons midi-output unwatch)
      obj
    (setf cc-nums
          (coerce
           (or (getf args :cc-nums)
               '(16 17 18 19 20 21 22 23 ;;; knobs
                 0 1 2 3 4 5 6 7         ;;; fader
                 32 33 34 35 36 37 38 39 ;;; s-buttons
                 48 49 50 51 52 53 54 55 ;;; m-buttons1
                 64 65 66 67 68 69 70 71 ;;; r-buttons
               
                 58 59        ;;; prev next
                 46 60 61 62  ;;; cycle, set, prev-mark, next-mark
                 43 44 42 41 45 ;;; transport: rew, ffwd, stop, play, rec
                 ))
           'vector))
;;;    (setf midi-output (cm:ensure-jackmidi midi-output))
    (loop
      for idx from 0
      for ccnum across cc-nums
      do (setf (aref cc-map ccnum) idx))
    (setf cc-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums)
                                     collect (cond
                                               ((< x 16) (make-ref 0.0))
;;;                                               ((< x 40) (make-bang (lambda ()) 0.0))
                                               (t  (make-bang nil 0.0))))))
    (setf nk2-faders (make-array 16 :displaced-to cc-state))
    (setf s-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 16))
    (setf m-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 24))
    (setf r-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 32))
    (map () (lambda (slot local-idx)
              (setf (slot-value obj slot) (aref cc-state local-idx)))
         '(track-left track-right
           cycle set-marker marker-left marker-right
           tr-rewind tr-ffwd tr-stop tr-play tr-rec)
         (v-collect (n 11) (+ n 40)))
    (loop
      for i from 16 below (length cc-nums) ;;; reflect the value of all buttons in the hardware LED.
      do (let* ((local-idx i)
                (cc-num (aref cc-nums local-idx))
                (local-pulsar (make-led-pulsar cc-num chan midi-output)))
           (push (watch (lambda ()
                          (let ((state (round (get-val (aref cc-state local-idx)))))
                            (case state
                              ((0 1) (funcall local-pulsar :stop)
                               (osc-midi-write-short
                                midi-output
                                (+ (1- chan) 176) cc-num (if (zerop state) 0 127)))
                              (2 (funcall local-pulsar :start))))))
                 unwatch)
           (set-val (aref cc-state local-idx) (get-val (aref (aref *midi-cc-state* (1- chan)) (aref cc-nums local-idx)))))))
  (update-hw-state obj))

(defmethod handle-midi-in ((instance nanoktl2-midi) opcode d1 d2)
  (with-slots (cc-fns cc-nums nk2-fader-update-fns
               nk2-fader-modes nk2-fader-last-cc
               hide-fader cc-map cc-state note-fn last-note-on midi-output chan)
      instance
    (case opcode
      (:cc (incudine.util:msg :info "ccin: ~a ~a" d1 d2)
       (let ((local-idx (aref cc-map d1))
             (d2-norm (/ d2 127)))
         (when local-idx
           (cond
             ((< local-idx 16)
              (let* ((fn (aref nk2-fader-update-fns local-idx))
                     (fader-mode (aref nk2-fader-modes local-idx))
                     (last-cc (aref nk2-fader-last-cc local-idx))
                     (gui-slot (aref cc-state local-idx)))
                (case fader-mode
                  (:scale
                   (progn
                     (unless hide-fader
                       (set-val gui-slot (buchla-scale d2-norm  last-cc (get-val gui-slot))))
                     (setf (aref nk2-fader-last-cc local-idx) d2-norm)))
                  (:jump (unless hide-fader (set-val gui-slot d2-norm)))
                  (:catch (unless hide-fader
                            (if fn
                                (when (funcall fn d2-norm (get-val gui-slot))
                                  (set-val gui-slot d2-norm)
                                  (setf (aref nk2-fader-update-fns local-idx) nil))
                                (set-val gui-slot d2-norm)))))))
             ;; ((<= 40 local-idx 45)
             ;;  (case local-idx
             ;;    (43 ;;; set button
             ;;     (setf hide-fader (> d2-norm 0)))
             ;;    (42 ;;; cycle button
             ;;     (update-hw-state instance))
             ;;    (otherwise
             ;;     (let ((slot (aref cc-state (aref cc-map d1))))
             ;;       (unless (zerop d2-norm) (trigger slot))))))
             (t (unless (zerop d2)
;;;                  (break "local-idx: ~a" local-idx)
                  (let ((slot (aref cc-state local-idx)))
                    (let ((*refs-seen* (list slot "bang")))
                      (%trigger slot)))))))))
      (:note-on (setf last-note-on d1)))))

(defmethod update-hw-state ((instance nanoktl2-midi))
  "Update the state of a Midicontroller Hardware by sending all values of
/instance/ to its midi-out port.

@Arguments

instance - An instance of a class or subclass of <<midi-controller>>.

@See-also

clamps:cl-midictl
"
  (with-slots (chan cc-nums cc-map cc-state midi-output) instance
    (loop
      for local-idx from 16 below (length cc-nums)
      do (let ((cc-num (aref cc-nums local-idx)))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 176) cc-num (round (get-val (aref cc-state local-idx))))))))
