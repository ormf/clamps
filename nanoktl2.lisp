;;; 
;;; nanoktl2.lisp
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
   (s-buttons :accessor s-buttons)
   (m-buttons :accessor m-buttons)
   (r-buttons :accessor r-buttons)
   (track-left :accessor track-left)
   (track-right :accessor track-right)
   (cycle :accessor cycle)
   (set-marker :accessor set-marker)
   (marker-left :accessor marker-left)
   (marker-right :accessor marker-right)
   (tr-rewind :accessor tr-rewind)
   (tr-ffwd :accessor tr-ffwd)
   (tr-stop :accessor tr-stop)
   (tr-play :accessor tr-play)
   (tr-rec :accessor tr-rec)
   (cc-nums :accessor cc-nums)))

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

|#

(defmethod initialize-instance :after ((obj nanoktl2-midi) &rest args)
  (with-slots (cc-map cc-nums cc-state chan nk2-faders s-buttons m-buttons r-buttons)
      obj
    (setf cc-nums
          (coerce
           (or (getf args :cc-nums)
               '(16 17 18 19 20 21 22 23        ;;; knobs
                 0 1 2 3 4 5 6 7                ;;; fader
                 32 33 34 35 36 37 38 39        ;;; s-buttons
                 48 49 50 51 52 53 54 55        ;;; m-buttons
                 64 65 66 67 68 69 70 71        ;;; r-buttons
                        
                 58 59              ;;; prev next
                 46 60 61 62 ;;; cycle, set, prev-mark, next-mark
                 43 44 42 41 45 ;;; transport: rew, ffwd, stop, play, rec
                 ))
           'vector))
    (dotimes (i 128) (setf (aref cc-map i) nil)) ;;; initialize cc-map with nil

    (loop
      for idx from 0
      for ccnum across cc-nums
      do (setf (aref cc-map ccnum) idx))
    (setf cc-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums) collect (if (<= 40 x 45)
                                                                              (make-instance 'bang-cell)
                                                                              (make-instance 'value-cell)))))
    (setf nk2-faders (make-array 16 :displaced-to cc-state))
    (setf s-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 16))
    (setf m-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 24))
    (setf r-buttons (make-array 8 :displaced-to cc-state :displaced-index-offset 32))
    (map () (lambda (slot local-idx)
              (setf (slot-value obj slot) (aref cc-state local-idx)))
         '(cl-midictl:track-left
           cl-midictl:track-right
           cl-midictl:cycle
           cl-midictl:set-marker
           cl-midictl:marker-left
           cl-midictl:marker-right
           cl-midictl:tr-rewind
           cl-midictl:tr-ffwd
           cl-midictl:tr-stop
           cl-midictl:tr-play
           cl-midictl:tr-rec)
         (v-collect (n 11) (+ n 40)))
    (dotimes (i (length cc-nums))
      (unless (<= 40 i 45)
        (setf (val (aref cc-state i)) (aref (aref *midi-cc-state* chan) (aref cc-nums i)))))
    )
  (update-state obj))

(defmethod handle-midi-in ((instance nanoktl2-midi) opcode d1 d2)
;;;  (call-next-method)
  (with-slots (cc-fns cc-nums nk2-fader-update-fns cc-map cc-state note-fn last-note-on midi-output chan) instance
    (case opcode
      (:cc (incudine.util:msg :info "ccin: ~a ~a" d1 d2)
       (let ((fader-idx (aref cc-map d1)))
         (when fader-idx
           (cond
             ((< fader-idx 16)
              (let* ((fn (aref nk2-fader-update-fns fader-idx))
                     (gui-slot (aref cc-state fader-idx)))
                (if fn
                    (when (funcall fn d2 (val gui-slot))
                      (setf (val gui-slot) d2)
                      (setf (aref nk2-fader-update-fns fader-idx) nil))
                    (setf (val gui-slot) d2))
                ))
             ((<= 40 fader-idx 45)
              (let ((slot (aref cc-state (aref cc-map d1))))
                (unless (zerop d2) (trigger slot nil))))
             (t (let ((slot (aref cc-state (aref cc-map d1))))
                  (toggle-slot slot)))))))
      (:note-on (setf last-note-on d1)))))

(defmethod update-state ((instance nanoktl2-midi))
  (with-slots (chan cc-nums cc-map cc-state midi-output) instance
    (dotimes (local-idx (length cc-nums))
      (unless (<= 40 local-idx 45)
        (let ((cc-num (aref cc-nums local-idx)))
          (osc-midi-write-short
           midi-output
           (+ chan 176) cc-num (val (aref cc-state local-idx))))))))

;;; (cellctl:set-ref)
#|
(defmethod (setf s-buttons) (val (obj nanoktl2) idx)
  (setf (aref (slot-value instance :s-buttons) idx) val)
  val)
|#

