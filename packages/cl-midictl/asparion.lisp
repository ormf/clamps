;;; 
;;; asparion.lisp
;;;
;;; Implementation of an Asparion MIDI Controller. The Asparion MIDI
;;; Controller is mostly compatible with the MACKIE Control
;;; Protocol. Differences are for the LED rings, which implement 15
;;; values instead of the 11 values of the MACKIE, the rotary colors,
;;; which don't exist in the MACKIE and 3 Lines with 12 characters in
;;; the displays. It should be fairly straightforward to change the
;;; code below to be used for a Mackie Controller.
;;;
;;; NOTE: In the Asparion Controller Software the Color Options have
;;; to be set to "midi-controlled", the checkboxes of the "send",
;;; "eq", "send" and "fx" buttons have to be unchecked and the LED
;;; mode of the rotaries has to be set from "Mackie" to "Normal" for
;;; the code below to work completely.
;;;
;;; The Asparion allows for using one D700ft unit with a variable
;;; number of D700 units (0-7). To accomodate this, the code below
;;; implements a class for the D700 and the D700FT each and a
;;; Metaclass for the Asparion itself. The number of units used gets
;;; indirectly specified in the :midi-ports argument of the object
;;; creation function of the asparion: The :midi-ports argument has to
;;; be a list of ids of the (already opened) midi-ports, the Asparion
;;; units are connected to, with the connection of the d700ft unit
;;; being the first in the list. The instances of the d700/d700ft
;;; units are automatically added in the initialize-instance method of
;;; the Asparion class with ids <Asparion-id>-d700ft,
;;; <Asparion-id>-d700 (in the case of two units), or
;;; <Asparion-id>-d700ft, <Asparion-id>-d700-0, <Asparion-id>-d700-1,
;;; etc. in the case of three or more units.
;;;
;;; The asparion instance combines the arrays for buttons, rotaries
;;; and faders of the d700/d700ft units to arrays of the total size of
;;; the number of channel strips to facilitate dealing with all strips
;;; in a straightforward and intuitive way. As the ref-cell elements
;;; of these arrays are directly linked to the respectice arrays of
;;; the d700/d700ft instances, value changes can be done either in the
;;; instances or in the asparion instance, resulting in the exact same
;;; behaviour.
;;;
;;; Here is an example of initalizing an Asparion with two units
;;; connected to MIDI ports :midi-1 and :midi-2
;;;
;;; (add-midi-controller 'asparion :asparion :midi-ports (:midi-1 :midi-2))
;;;
;;; After the controller is created, (list-midi-ports) should return
;;; (:asparion :asparion-d700 :asparion-d700ft)
;;;
;;; Removing the Asparion with (remove-midi-controller :asparion) will
;;; also remove the d700ft and d700 instances.
;;;
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

(defparameter *asparion-sysex-header* '(#xF0 #x00 #x00 #x66 #x14))

(defclass d700 (midi-controller)
  ((strip-labels :initarg :strip-labels :accessor strip-labels
                 :initform (apply #'vector
                                  (loop for i below 8
                                        collect
                                        (vector (make-ref (format nil "~d" (1+ i))) (make-ref "") (make-ref ""))))
                 :documentation "labels of strips in gui")
   (fader-labels :initarg :fader-labels :accessor fader-labels
                 :initform t
                 :documentation "flag indicating whether to echo the fader values in line 2 of the display.")
   (rotary-labels :initarg :rotary-labels :accessor rotary-labels
                 :initform t
                  :documentation "flag indicating whether to echo the rotary values in line 3 of the display.")
   (rotary-ring :initarg :rotary-ring :accessor rotary-ring
                 :initform t
                 :documentation "flag indicating whether to echo the rotary values in rotary ring leds.")
   (vu-meters :initarg :vu-meters :accessor vu-meters
                 :initform (apply #'vector (loop repeat 8 collect (make-ref 0.0)))
                 :documentation "labels of strips in gui.")
   (rotary-a :accessor rotary-a
             :initform (apply #'vector (loop repeat 8 collect (make-ref 0.0))))
   (rotary-led-modes :accessor rotary-led-modes
                    :initform (apply #'vector (loop repeat 8 collect (make-ref 0))))
   (rotary-colors :accessor rotary-colors
                  :initform (apply #'vector (loop repeat 8 collect (make-ref "ffff00")))
                  :documentation "RGB colors of the 8 rotary buttons.")
   (rotary-scale :accessor rotary-scale
                 :initform (apply #'vector (loop repeat 8 collect 0.01))
                 :documentation "scaling factor of the 8 rotary buttons")
   (rotary-buttons :accessor rotary-buttons
                   :initform (apply #'vector (loop repeat 8 collect (make-bang nil 0.0)))
                   :documentation "action associated with pressing the rotary button.")
   (midi-in-active :accessor midi-in-active
                   :initform nil
                   :documentation "flag indicating that midi-input is occuring.")
   (faders :accessor faders
           :initform (apply #'vector (loop repeat 8 collect (make-ref 0.0)))
           :documentation "value of the 8 faders.")
   (fadertouch :accessor fadertouch
               :initform (apply #'vector (loop repeat 8 collect (make-ref 0)))
               :documentation "ref-cells indicating if faders are currently touched (0/1).")
   (sel-buttons :accessor sel-buttons
                :initform (apply #'vector (loop repeat 8 collect (make-bang nil 0.0)))
                :documentation "action associated with pressing the Select buttons")
   (s-buttons :accessor s-buttons
              :initform (apply #'vector (loop repeat 8 collect (make-bang nil 0.0)))
              :documentation "action associated with pressing the Solo buttons")
   (m-buttons :accessor m-buttons
              :initform (apply #'vector (loop repeat 8 collect (make-bang nil 0.0)))
              :documentation "action associated with pressing the Mute buttons")
   (r-buttons :accessor r-buttons
              :initform (apply #'vector (loop repeat 8 collect (make-bang nil 0.0)))
              :documentation "action associated with pressing the Rec buttons"))
  (:documentation "Class for an Asparion D700 Controller with Display, based on the
<<midi-controller>> class.

In addition to the slots of a <<midi-controller>> instance, d700
implements the following slots with accessor methods of the same name
and initargs being the keywords of the slot symbol:

@Arguments
strip-labels - Array of 8 elements containing an Array of 3 elements containing <<ref-object><ref-objects>> with strings of the text display lines of the D700.

vu-meters - Array of 8 elements containing <<ref-object><ref-objects>> of the VU meter value in the range [0..1].

faders - Array of 8 elements containing the <<ref-object><ref-objects>> of the 8 faders of the D700.

fadertouch - Array of 8 elements containing the <<ref-object><ref-objects>> of the 8 faders of the D700.

rotary-a - Array of 8 elements containing the <<ref-object><ref-objects>> of the 8 rotaries of the D700.

rotary-scale - Array of 8 elements containing the scaling factors of the 8 rotaries of the D700.

rotary-buttons - Array of 8 elements containing the <<bang-object><bang-objects>> of the 8 rotary push buttons of the D700.

rotary-colors - Array of 8 elements containing the <<ref-object><ref-objects>> of the colors of the 8 rotary push buttons of the D700. Colors are in the format \"00ff00\", indicating the hex values for R, G and B.

sel-buttons - Array of 8 <<bang-object><bang-objects>> containing the highlight state of the 8 Select buttons with a value of 0 or 1 and an associated trigger action when pushed.

s-buttons - Array of 8 <<bang-object><bang-objects>> containing the highlight state of the 8 Solo buttons with a value of 0 or 1 and an associated trigger action when pushed.

m-buttons - Array of 8 <<bang-object><bang-objects>> containing the highlight state of the 8 Mute buttons with a value of 0 or 1 and an associated trigger action when pushed.

r-buttons - Array of 8 <<bang-object><bang-objects>> containing the highlight state of the 8 Rec buttons with a value of 0 or 1 and an associated trigger action when pushed.

@See-also
midi-controller
asparion
d700ft
"))

(defun d700-write-line-1 (text idx stream)
  "Set text of the first line at strip /idx/ of the Display of an
Asparion <<D700>> or <<D700FT>> Controller connected to /stream/ to /text/.

@Arguments
/text/ - String of maximum 12 Characters.
/idx/ - Integer in the range [0..7] indicating the Channel strip of the display.
/stream/ - Jackmidi output stream connected to an Asparion Controller.

@See-also
d700-write-line2
d700-write-line3
"
  (midiout-sysex (append *asparion-sysex-header* `(#x1a ,(* 12 (mod idx 8)) 1) (map 'list #'char-code text) '(#xF7))
                 stream))

;;; (d700-write-line-1 "Hi" 0 (midi-port-out (find-midi-port :midi-1)))

(defun d700-write-line-2 (text idx stream)
  "Set text of the second line at strip /idx/ of the Display of an
Asparion <<D700>> or <<D700FT>> Controller connected to /stream/ to /text/.

@Arguments
/text/ - String of maximum 12 Characters.
/idx/ - Integer in the range [0..7] indicating the Channel strip of the display.
/stream/ - Jackmidi output stream connected to an Asparion Controller.

@See-also
d700-write-line1
d700-write-line3
"
  (midiout-sysex (append *asparion-sysex-header* `(#x1a ,(* 12 (mod idx 8)) 2) (map 'list #'char-code text) '(#xF7))
                 stream))

;;; (d700-write-line-2 "Hi" 1 (midi-port-out (find-midi-port :midi-1)))

(defun d700-write-line-3 (text idx stream)
    "Set text of the third line at strip /idx/ of the Display of an
Asparion <<D700>> or <<D700FT>> Controller connected to /stream/ to /text/.

@Arguments
/text/ - String of maximum 12 Characters.
/idx/ - Integer in the range [0..7] indicating the Channel strip of the display.
/stream/ - Jackmidi output stream connected to an Asparion Controller.

@See-also
d700-write-line1
d700-write-line2
"

  (midiout-sysex (append *asparion-sysex-header* `(#x19 ,(* 8 (mod idx 8))) (map 'list #'char-code text) '(#xF7))
                 stream))

;;; (d700-write-line-3 "Hallo" 1 (midi-port-out (find-midi-port :midi-1)))

(defun asparion-send-color (color strip-idx stream)
  "Send /color/ to a rotary at /strip-idx/ of an Asparion D700 or D700FT
Controller connected to MIDI output /stream/.

@Arguments
color - String with 8 bit Hex Values for R, G and B.
stream - Jackmidi output stream.
strip-idx - Integer denoting the index of the rotary strip (On the D700FT, 8 denotes Master Rotary).

@Example
(add-midi-controller 'd700 :d700)

;;; set the color of the leftmost rotary of the Asparion to red:

(let ((controller (find-controller :d700)))
  (asparion-send-color \"FF0000\" (midi-output controller) 0))

@See-also
asparion
d700
d700ft
"
  #| colors of rotaries are set by sending noteon of midinote (+ 32
  strip-idx) with velocity 0/127 on channel 0 for on/off and the color
  values as velocities on the channels 1-3 for R,G and B |#
  
  (osc-midi-write-short ;;; turn LED on
            stream
            #x90 
            (+ 32 strip-idx)
            127)
  (loop for rgb in (color->midi-rgb color)
        for chan from 1
        do (osc-midi-write-short ;;; set the colors
            stream
            (+ #x90 chan)
            (+ 32 strip-idx)
            rgb)))

(defmethod initialize-instance :after ((obj d700) &rest args)
  (declare (ignorable args))
    #|
    '(16 17 18 19 20 21 22 23   ;;; rotaries (cc)
       0  1  2  3  4  5  6  7   ;;; fader (pitch-bend on channel)
      32 33 34 35 36 37 38 39   ;;; rotary-buttons (noteon/off)
      16 49 50 51 52 53 54 55   ;;; m-buttons (noteon/off)
       0  1  2  3  4  5  6  7   ;;; r-buttons (noteon/off)
       8  9 10 11 12 13 14 15   ;;; s-buttons (noteon/off)
      24 25 26 27 28 29 30 31   ;;; sel-buttons (noteon/off)
    )
    |#
  (with-slots (echo cc-map vu-meters strip-labels rotary-a rotary-led-modes rotary-colors rotary-buttons rotary-scale chan faders m-buttons r-buttons s-buttons sel-buttons
               midi-port midi-output midi-in-active unwatch fader-labels rotary-labels rotary-ring)
      obj
    (loop for cc-num in '(16 17 18 19 20 21 22 23) ;;; rotaries
          for idx from 0
          do (setf (aref cc-map cc-num) idx))
    (dotimes (i 8)
      (let ((i i))
        (add-trigger-fn (aref rotary-buttons i)
                        (lambda () (setf (aref rotary-scale i)
                                    (if (= 0.01 (aref rotary-scale i))
                                        0.001 0.01))))
        (dolist (sym '(r-buttons s-buttons m-buttons sel-buttons))
          (add-trigger-fn (aref (slot-value obj sym) i) (lambda () (toggle-slot (aref (slot-value obj sym) i)))))
        (push (watch (lambda ()
                       (when echo
                         (d700-write-line-1 (format nil "~12a" (get-val (aref (aref strip-labels i) 0))) i midi-output))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (d700-write-line-2 (format nil "~12a" (get-val (aref (aref strip-labels i) 1))) i midi-output))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (d700-write-line-3 (format nil "~12a" (get-val (aref (aref strip-labels i) 2))) i midi-output))))
              unwatch)
        (push (watch (lambda ()
                       (let ((val (get-val (aref rotary-a i))))
                         (when (and echo rotary-ring)
                           (osc-midi-write-short
                            midi-output
                            (+ 1 (get-val (aref rotary-led-modes i)) 176)
                            (+ 48 i) (round (* 127 val))))
                         (when rotary-labels (set-val (aref (aref strip-labels i) 2) (format nil "~,3f" val))))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (asparion-send-color
                          (get-val (aref rotary-colors i))
                          i
                          midi-output))))
              unwatch)
        (push (watch (lambda ()
                       (let* ((val (get-val (aref faders i)))
                              (bendval (normalized->bendvalue val)))
                         (when (and echo (not midi-in-active))
                           (osc-midi-write-short
                            midi-output
                            (+ i #xe0)
                            (logand bendval 127) (ash bendval -7)))
                         (incudine.util:msg :info "setting Fader-idx: ~a" i)
                         (when fader-labels (set-val (aref (aref strip-labels i) 1) (format nil "~,3f" val))))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (osc-midi-write-short
                          midi-output
                          (+ (1- chan) 144)
                          i (if (zerop (get-val (aref r-buttons i))) 0 127)))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (osc-midi-write-short
                          midi-output
                          (+ (1- chan) 144)
                          (+ 8 i) (if (zerop (get-val (aref s-buttons i))) 0 127)))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (osc-midi-write-short
                          midi-output
                          (+ (1- chan) 144)
                          (+ 16 i)
                          (if (zerop (get-val (aref m-buttons i))) 0 127)))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (osc-midi-write-short
                          midi-output
                          (+ (1- chan) 144)
                          (+ 24 i) (if (zerop (get-val (aref sel-buttons i))) 0 127)))))
              unwatch)
        (push (watch (lambda ()
                       (when echo
                         (jackmidi:write-short
                          midi-output
                          (jackmidi:message #xd0 (+ (* i 16) (round (* 12 (get-val (aref vu-meters i)))))) 2))))
              unwatch)))))

(defun decode-text (vector)
  (map 'string #'code-char vector))

(defmethod handle-midi-in ((instance d700) opcode channel d1 d2)
  (incudine.util:msg :info "d700 input: ~a ~a ~a ~a" opcode channel d1 d2)
  (with-slots (chan rotary-a rotary-buttons rotary-scale faders
               fadertouch s-buttons m-buttons r-buttons sel-buttons
               midi-input midi-in-active strip-labels)
      instance
    (setf midi-in-active t)
    (case opcode
      (:pitch-bend
       (unless (< channel 8) (error "illegal fader channel 8 for D700 instance. Did you intend to
instantiate a D700FT instance instead or mismatched the midi-ports of the devices?"))
       (let ((fader-idx channel))
         (set-val (aref faders fader-idx)
                  (float (/ (+ d1 (ash d2 7)) (1- (ash 1 14)))))))
      (:cc
       (when (and (= chan (1+ channel)) (<= 16 d1 23))
         (let ((rotary-idx (- d1 16)))
           (let* ((old (get-val (aref rotary-a rotary-idx)))
                  (new (clip (+ old
                                (* (aref rotary-scale rotary-idx)
                                   (midi->inc d2)))
                             0 1)))
             (when (/= old new)
               (set-val (aref rotary-a rotary-idx) new))))))
      (:note-on
       (when (= chan (1+ channel))
         (cond
           ((<= 32 d1 39) (trigger (aref rotary-buttons (- d1 32))))
           ((<= 0 d1 7) (trigger (aref r-buttons d1)))
           ((<= 8 d1 15) (trigger (aref s-buttons (- d1 8))))
           ((<= 16 d1 23) (trigger (aref m-buttons (- d1 16))))
           ((<= 24 d1 31) (trigger (aref sel-buttons (- d1 24))))
           ((<= 104 d1 111) (set-val (aref fadertouch (- d1 104)) 1)))))
      (:note-off
       (when (= chan (1+ channel))
         (cond
           ;; ((<= 32 d1 39) (trigger (aref rotary-buttons (- d1 32))))
           ;; ((<= 0 d1 7) (trigger (aref r-buttons d1)))
           ;; ((<= 8 d1 15) (trigger (aref s-buttons (- d1 8))))
           ;; ((<= 16 d1 23) (trigger (aref m-buttons (- d1 16))))
           ;; ((<= 24 d1 31) (trigger (aref sel-buttons (- d1 24))))
           ((<= 104 d1 111) (set-val (aref fadertouch (- d1 104)) 0)))))
      (:sysex
       (let* ((message (jackmidi:input-stream-sysex-octets midi-input))
              (text (decode-text (subseq message 7 (1- (length message)))))
              (strip-idx (round (/ (mod (aref message 6) 56) 7)))
              (line-idx (floor (aref message 6) 56)))
         (set-val (aref (aref strip-labels strip-idx) line-idx) text)
         ;; (incudine.util:msg :warn message)
         )
       ))
    (setf midi-in-active nil)))

(defmethod update-hw-state ((obj d700))
  "Update the state of the hardware controller by sending the current
state to all elements of the controller via midi."
  (with-slots (echo midi-output chan strip-labels
               rotary-a rotary-colors rotary-led-modes
               faders s-buttons m-buttons r-buttons sel-buttons)
      obj
    (let ((tmp echo))
      (setf echo t)
      (dotimes (i 8)
        (d700-write-line-1 (format nil "~12a" (get-val (aref (aref strip-labels i) 0))) i midi-output)
        (d700-write-line-2 (format nil "~12a" (get-val (aref (aref strip-labels i) 1))) i midi-output)  
        (d700-write-line-3 (format nil "~12a" (get-val (aref (aref strip-labels i) 2))) i midi-output)
        (let ((val (get-val (aref rotary-a i))))
          (osc-midi-write-short
           midi-output
           (+ 1 (get-val (aref rotary-led-modes i)) 176)
           (+ 48 i) (round (* 127 val))))
        (asparion-send-color (get-val (aref rotary-colors i)) i midi-output)
        (let* ((val (get-val (aref faders i)))
               (bendval (normalized->bendvalue val)))
          (osc-midi-write-short
           midi-output
           (+ i #xe0)
           (logand bendval 127) (ash bendval -7)))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 144)
         i (if (zerop (get-val (aref r-buttons i))) 0 127))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 144)
         (+ 8 i) (if (zerop (get-val (aref s-buttons i))) 0 127))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 144)
         (+ 16 i) (if (zerop (get-val (aref m-buttons i))) 0 127))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 144)
         (+ 24 i) (if (zerop (get-val (aref sel-buttons i))) 0 127)))
      (setf echo tmp))))


;;; stubs: redefine to bind actions to the buttons.

(defun asparion-pan-press-action ())
(defun asparion-eq-press-action ())
(defun asparion-send-press-action ())
(defun asparion-fx-press-action ())
(defun asparion-star-press-action ())
(defun asparion-metronome-press-action ())
(defun asparion-loop-press-action ())
(defun asparion-rec-press-action ())
(defun asparion-play-press-action ())
(defun asparion-stop-press-action ())
(defun asparion-prev-page-press-action ())
(defun asparion-next-page-press-action ())
(defun asparion-master-rotary-press-action ())

(defclass d700ft (d700)
  ((pan-button :accessor pan-button
               :initform (make-bang #'asparion-pan-press-action 0.0)
               :documentation "action associated with pressing the Pan button")
   (eq-button :accessor eq-button
              :initform (make-bang #'asparion-eq-press-action 0.0)
              :documentation "action associated with pressing the Eq button")
   (send-button :accessor send-button
                :initform (make-bang #'asparion-send-press-action 0.0)
                :documentation "state of the the Send button")
   (fx-button :accessor fx-button
              :initform (make-bang #'asparion-fx-press-action 0.0)
              :documentation "action associated with pressing the Fx button")
   (star-button :accessor star-button
                :initform (make-bang #'asparion-star-press-action 0.0)
                :documentation "action associated with pressing the Star button")
   (metronome-button :accessor metronome-button
                     :initform (make-bang #'asparion-metronome-press-action 0.0)
                     :documentation "action associated with pressing the Metronome button")
   (loop-button :accessor loop-button
                :initform (make-bang #'asparion-loop-press-action 0.0)
                :documentation "action associated with pressing the Loop button")
   (rec-button :accessor rec-button
               :initform (make-bang #'asparion-rec-press-action 0.0)
               :documentation "action associated with pressing the Rec button")
   (play-button :accessor play-button
                :initform (make-bang #'asparion-play-press-action 0.0)
                :documentation "action associated with pressing the Play button")
   (stop-button :accessor stop-button
                :initform (make-bang #'asparion-stop-press-action 0.0)
                :documentation "action associated with pressing the Stop button")
   (prev-page-button :accessor prev-page-button
                     :initform (make-bang #'asparion-prev-page-press-action 0.0)
                     :documentation "action associated with pressing the Prev-Page button")
   (next-page-button :accessor next-page-button
                     :initform (make-bang #'asparion-next-page-press-action 0.0)
                     :documentation "action associated with pressing the Next-Page button")
   (master-rotary :accessor master-rotary
                  :initform (make-bang #'asparion-master-rotary-press-action 1.0)
                  :documentation "action associated with pressing the Master button")
   (master-rotary-scale :accessor master-rotary-scale
                  :initform (make-ref (/ 127.0))
                        :documentation "scaling factor of the Master rotary")
   (master-rotary-color :accessor master-rotary-color
                  :initform (make-ref "ffff00")
                  :documentation "RGB color of the Master rotary"))
  (:documentation "Class for an Asparion D700FT Controller with Display, based on the
d700 class.

In addition to the slots of a <<d700>> instance, d700ft implements the
following slots with accessor methods of the same name and initargs
being the keywords of the slot symbol:

@Arguments
pan-button - <<bang-object><bang-object>> of the pan button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed.

eq-button - <<bang-object><bang-object>> of the eq button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

send-button - <<bang-object><bang-object>> of the send button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

fx-button - <<bang-object><bang-object>> of the fx button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

star-button - <<bang-object><bang-object>> of the star button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

metronome-button - <<bang-object><bang-object>> of the metronome button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

loop-button - <<bang-object><bang-object>> of the loop button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

rec-button - <<bang-object><bang-object>> of the rec button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

play-button - <<bang-object><bang-object>> of the play button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

stop-button - <<bang-object><bang-object>> of the stop button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

prev-page-button - <<bang-object><bang-object>> of the previous page button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

next-page-button - <<bang-object><bang-object>> of the next page button of the D700FT indicating the highlight state (0 or 1) and an associated trigger action when pushed..

master-rotary - <<bang-object>> of the master rotary of the D700FT conatining its vlaue in the range [0..1] and an associated trigger action when pushed.

master-rotary-scale - <<ref-object>> of the master rotary scaling factor of the D700FT.

master-rotary-color - <<ref-object>> of the master rotary RGB color of the D700FT.

@See-also
midi-controller
asparion
d700
"))

(defmethod initialize-instance :after ((obj d700ft) &rest args)
  (declare (ignorable args))
  #| for the common controllers and buttons see the
     initialize-instance method of the d700. The midinotes of the
     additional buttons of the d700ft can be seen in the mapping list
     below. As in the d700 the default trigger function of the buttons
     is to toggle the button light. |#
  (with-slots (echo pan-button eq-button send-button fx-button star-button metronome-button loop-button rec-button play-button stop-button prev-page-button next-page-button master-rotary master-rotary-color chan midi-port midi-output midi-in-active unwatch)
      obj
    (dolist (mapping '((pan-button 42) (eq-button 44) (send-button 41) (fx-button 43) (star-button 54)
                       (metronome-button 89) (loop-button 86)
                       (rec-button 95) (play-button 94) (stop-button 93)
                       (prev-page-button 46) (next-page-button 47)))
      (incudine.util:msg :info "midi-out: ~a" midi-output)
      (destructuring-bind (button-sym keynum) mapping
        (add-trigger-fn (slot-value obj button-sym) (lambda () (toggle-slot (slot-value obj button-sym))))
        (push (watch (lambda ()
                       (when echo
                         (osc-midi-write-short
                          midi-output
                          (+ (1- chan) #x90)
                          keynum (if (zerop (get-val (slot-value obj button-sym))) 0 127)))))
              unwatch))
      (push (watch (lambda ()
                     (when echo
                       (set-val master-rotary-color
                                (format nil "00~2,'0X00" (round (* 127 (get-val master-rotary))))))))
            unwatch)
      (push (watch (lambda ()
                     (when echo
                       (asparion-send-color (get-val master-rotary-color) 24 midi-output))))
            unwatch))))

(defmethod handle-midi-in ((instance d700ft) opcode channel d1 d2)
  (incudine.util:msg :info "d700ft input: ~a ~a ~a ~a" opcode channel d1 d2)
  (with-slots (strip-labels pan-button eq-button send-button fx-button star-button
               metronome-button loop-button
               rec-button play-button stop-button prev-page-button next-page-button
               rotary-a rotary-buttons rotary-scale faders fadertouch
               s-buttons m-buttons r-buttons sel-buttons
               midi-input midi-output midi-in-active chan master-rotary)
      instance
    (setf midi-in-active t)
    (case opcode
      (:pitch-bend
       (let ((fader-idx channel))
         (if (= channel 8)
             (set-val master-rotary (float (/ d2 127)))
             (progn
               (setf midi-in-active t)
               (set-val (aref faders fader-idx)
                        (float (/ (+ d1 (ash d2 7)) (1- (ash 1 14)))))
               (setf midi-in-active nil)))))
      (:cc
       (when (and (= chan (1+ channel)) (<= 16 d1 23))
         (let ((rotary-idx (- d1 16)))
           (let* ((old (get-val (aref rotary-a rotary-idx)))
                  (new (clip (+ old
                                (* (aref rotary-scale rotary-idx)
                                   (midi->inc d2)))
                             0 1)))
             (when (/= old new)
               (set-val (aref rotary-a rotary-idx) new))))))
      (:note-on
       (when (= chan (1+ channel))
         (incudine.util:msg :info "note-on: ~a ~a ~a ~a" opcode channel d1 d2)
         (cond
           ((<= 0 d1 7) (trigger (aref r-buttons d1)))
           ((<= 8 d1 15) (trigger (aref s-buttons (- d1 8))))
           ((<= 16 d1 23) (trigger (aref m-buttons (- d1 16))))
           ((<= 24 d1 31) (trigger (aref sel-buttons (- d1 24))))
           ((<= 32 d1 39) (trigger (aref rotary-buttons (- d1 32))))
           ((<= 104 d1 111) (set-val (aref fadertouch (- d1 104)) 1))

           (t
            (case d1
              (41 (trigger send-button))
              (42 (trigger pan-button))
              (43 (trigger fx-button))
              (44 (trigger eq-button))
              (46 (trigger prev-page-button))
              (47 (trigger next-page-button))
              (54 (trigger star-button))
              (86 (trigger loop-button))
              (89 (incudine.util:msg :info "metronome")
               (trigger metronome-button)
               (incudine.util:msg :info "metronome"))
              (93 (trigger stop-button))
              (94 (trigger play-button))
              (95 (trigger rec-button))
              (otherwise
               (incudine.util:msg :info "slipped through: note-on: ~a ~a ~a ~a ~a" opcode channel d1 d2 (eq d1 40))))))))
      (:note-off
       (when (= chan (1+ channel))
         (incudine.util:msg :info "note-off: ~a ~a ~a ~a" opcode channel d1 d2)
         (cond
           ;; ((<= 0 d1 7) (trigger (aref r-buttons d1)))
           ;; ((<= 8 d1 15) (trigger (aref s-buttons (- d1 8))))
           ;; ((<= 16 d1 23) (trigger (aref m-buttons (- d1 16))))
           ;; ((<= 24 d1 31) (trigger (aref sel-buttons (- d1 24))))
           ;; ((<= 32 d1 39) (trigger (aref rotary-buttons (- d1 32))))
           ((<= 104 d1 111) (set-val (aref fadertouch (- d1 104)) 0)))))
      (:sysex
       (let* ((message (jackmidi:input-stream-sysex-octets midi-input))
              (text (decode-text (subseq message 7 (1- (length message)))))
              (strip-idx (round (/ (mod (aref message 6) 56) 7)))
              (line-idx (floor (aref message 6) 56)))
         (set-val (aref (aref strip-labels strip-idx) line-idx) text)
;;;         (incudine.util:msg :warn message)
         )
       ))
    (setf midi-in-active nil)))


(defmethod update-hw-state ((obj d700ft))
  "Update the state of the hardware controller by sending the current
state to all elements of the controller via midi."
  (with-slots (echo midi-output chan strip-labels
               rotary-a rotary-colors rotary-led-modes
               faders s-buttons m-buttons r-buttons sel-buttons)
      obj
    (call-next-method)
    (let ((tmp echo))
      (setf echo t)
      (with-slots (pan-button eq-button send-button fx-button star-button metronome-button
                   loop-button rec-button play-button stop-button prev-page-button next-page-button chan
                   midi-port midi-output)
          obj
        (dolist (mapping '((pan-button 42) (eq-button 44) (send-button 41) (fx-button 43) (star-button 54)
                           (metronome-button 89) (loop-button 86)
                           (rec-button 95) (play-button 94) (stop-button 93)
                           (prev-page-button 46) (next-page-button 47)))
          (destructuring-bind (button-sym keynum) mapping
            (osc-midi-write-short
             midi-output
             (+ (1- chan) #x90)
             keynum (if (zerop (get-val (slot-value obj button-sym))) 0 127)))))
      (setf echo tmp))))

(defclass asparion (d700ft)
  ((midi-ports :initarg :midi-ports :initform nil :accessor midi-ports
            :documentation
            "The midi ports of the D700(FT) units of an asparion instance.")
   (units :initform nil
            :documentation
            "Slot containing the instances of the D700(FT) units of an asparion
instance."))
  (:documentation "Class for an Asparion midi controller with a /D700FT/ and 0-7
/D700/ extensions and their displays. The extensions are autocreated
as <<midi-controller><midi-controllers>> of classes <<d700>> and
<<d700ft>>, with the given /midi-ports/ assigned to these
instances (<<d700ft>> first) and the ids /<asparion-id>-d700ft/ and
/<asparion-id>-d700/. If more than 2 units are used, the ids of the
<<d700>> units are named /<asparion-id>-d700-1/,
/<asparion-id>-d700-2/, etc.

Asparion implements all slots of the <<d700ft>> class with the array
sizes of the buttons, faders, rotaries, vu-meters and strip-labels
adjusted to the total size of channel strips. Changing the value of
the ref-objects or calling the trigger-functions of the bang-objects
in the asparion instance is identical to changing/calling the objects
in the instances of the autocreated <<d700ft>> and <<d700>> units.

In addition to the <<d700ft>> slots, the asparion class defines the
following slots with accessor methods of the same name and initargs
being the keywords of the slot symbol:

NOTE: The :midi-ports keyword argument is mandatory, as the instance
relies on it to initialize its slots and behaviours.

@Arguments
midi-ports - List of Keywords denoting the ids of the Midi Ports of the D700(FT) units.

units - List containing the instances of the <<d700>>/<<d700ft>> units. Autocreated, therefore no accessor function.

@Example

;;; Create an Asparion with a D700FT and D700 unit connected to the
;;; Midi Ports :midi-1 and :midi-2

(add-midi-controller 'asparion :asparion :midi-ports '(:midi-1 :midi-2))
;;; output in REPL:
;;; adding midi controller :asparion
;;; adding midi controller :asparion-d700ft
;;; adding midi controller :asparion-d700

(list-midi-controllers)
;;; -> (:asparion :asparion-d700 :asparion-d700ft)

(remove-midi-controller :asparion)
;;; => nil

(list-midi-controllers)
;;; => nil

;;; Create an Asparion with a D700FT and two D700 units connected to the
;;; Midi Ports :midi-1, :midi-2 and :midi-3

(add-midi-controller 'asparion :asparion :midi-ports '(:midi-1 :midi-2 :midi-3)) ;;;  => #<asparion {1200DB0343}>
;;; output in REPL:
;;; adding midi controller :asparion
;;; adding midi controller :asparion-d700ft
;;; adding midi controller :asparion-d700-1
;;; adding midi controller :asparion-d700-2

(list-midi-controllers)
 ; => (:asparion :asparion-d700-1 :asparion-d700-2 :asparion-d700ft)


@See-also
d700
d700ft
midi-controller"))

(defmethod initialize-instance :after ((obj asparion) &rest args)
  (declare (ignorable args))
  (with-slots (echo midi-ports units

               pan-button eq-button send-button fx-button star-button
               metronome-button loop-button
               rec-button play-button stop-button prev-page-button next-page-button

               rotary-a rotary-buttons rotary-scale
               rotary-colors rotary-led-modes
               rotary-labels rotary-ring
               
               faders fadertouch
               fader-labels

               midi-input midi-output midi-in-active chan master-rotary

               cc-map cc-fns cc-state note-state keynum-map
               strip-labels vu-meters
               
               sel-buttons s-buttons m-buttons r-buttons unwatch)
      obj
    (map nil #'funcall unwatch)
    (setf unwatch nil)
    (setf keynum-map nil)
    (setf cc-map nil)
    (setf cc-fns nil)
    (setf cc-state nil)
    (setf note-state nil)
    ;;; remove obj from midi-input hashtable
    (setf (gethash midi-input *midi-controllers*)
          (delete obj (gethash (midi-input obj) *midi-controllers*)))
    (setf midi-input nil)
    (setf midi-output nil)
    (setf units
          (loop
            for module-idx from 0
            for midi-port in midi-ports
            collect (cond
                      ((zerop module-idx)
                       (incudine.util:msg :warn "midi-port of D700FT: ~a" midi-port)
                       (add-midi-controller 'd700ft (make-keyword (format nil "~@:(~a-d700ft~)" (mctl-id obj)))
                                            :echo echo :midi-port midi-port
                                            :rotary-ring rotary-ring :rotary-labels rotary-labels :fader-labels fader-labels
                                            ))
                      ((= (length midi-ports) 2)
                       (incudine.util:msg :warn "midi-port of D700: ~a" midi-port)
                       (add-midi-controller 'd700 (make-keyword (format nil "~@:(~a-d700~)" (mctl-id obj)))
                                            :echo echo :midi-port midi-port
                                            :rotary-ring rotary-ring :rotary-labels rotary-labels :fader-labels fader-labels
                                            ))
                      (t
                       (add-midi-controller 'd700 (make-keyword (format nil "~@:(~a-d700-~d~)" (mctl-id obj) module-idx))
                                            :midi-port midi-port)))))
    (let ((num-faders (* 8 (length midi-ports)))) ;;; reinit arrays with the size of all faders.
      (dolist (sym '(rotary-colors rotary-a rotary-led-modes rotary-scale rotary-buttons
                     faders fadertouch sel-buttons s-buttons
                     m-buttons r-buttons vu-meters strip-labels))
        (setf (slot-value obj sym) (make-array num-faders))))
    (loop ;;; map the arrays of the controller items of the asparion's
          ;;; units to the reinitialized arrays from above.
      for idx from 0
      for unit in units
      for startidx = (* idx 8)
      do (loop
           for sourceidx below 8
           for i from startidx
           do  (dolist (slot '(rotary-colors rotary-a rotary-led-modes
                               rotary-scale rotary-buttons faders fadertouch
                               sel-buttons s-buttons m-buttons r-buttons
                               vu-meters strip-labels))
                 (setf (aref (slot-value obj slot) i)
                       (aref (slot-value unit slot) sourceidx)))))
    (dotimes (i (* 8 (length midi-ports)))
      (set-val (aref (aref strip-labels i) 0) (format nil "~2,'0d" (1+ i))))
    (when echo
      (dolist (unit units) (update-hw-state unit)))))

(defmethod cleanup ((instance asparion))
  (dolist (unit (slot-value instance 'units))
    (remove-midi-controller (mctl-id unit))))

(defmethod handle-midi-in ((instance asparion) opcode channel d1 d2)
  "dummy method to avoid calling the d700ft handle-midi-in method."
  (declare (ignore opcode channel d1 d2)))

(defmethod update-hw-state ((obj asparion))
  "Delegate updating of the state of the controller hardware to the units
of the asparion instance."
  (with-slots (units) obj
    (map '() #'update-hw-state units)))

(defun final-digits (str &key (num-digits 3))
  "Return the terminating digits of string up to num-digits."
  (let ((len (length str)))
    (if (and (not (string= str "")) (numberp (read-from-string (subseq str (1- len)))))
        (cl-ppcre:regex-replace "[^0-9]+(.+)$" str "\\1" :start (max 0 (- len num-digits)))
        "")))

(defun trim-txt (str width)
  "Trim string to length width, keeping up to three terminating digits."
  (let* ((final-digits (final-digits str))
         (d-len (length final-digits)))
    (format nil "~a~a" (subseq str 0 (max 0 (- (min width (length str)) d-len))) final-digits)))

;;; (trim-txt "blah-123" 5)


(export
 '(asparion
   d700 d700ft

   strip-labels
   rotary-a rotary-led-modes rotary-scale rotary-colors rotary-buttons
   sel-buttons s-buttons m-buttons r-buttons
   vu-meters faders fadertouch
   pan-button eq-button
   send-button fx-button star-button
   metronome-button loop-button rec-button
   play-button stop-button prev-page-button
   next-page-button
   master-rotary master-rotary-scale master-rotary-color
   asparion-send-color
   )
 'cl-midictl)

;;;(defparameter *mackie-sysex-header* '(#xF0 #x00 #x00 #x66 #x10))
