;;; 
;;; faderfox.lisp
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

(defclass faderfox-midi (midi-controller)
  ((cc-nums :accessor cc-nums)
   (ff-faders :accessor ff-faders)
   (ff-buttons :accessor ff-buttons)))

;;; the cc-map maps the cc nums to the array slots. Their order is like this:
#|
array-idx: cc-nums of nanokontrol2

32-47:     cc-nums of faders
32-47:     keynums of buttons

the cc-map below is according to the factory settings of a
nanokontrol2.

|#

(defmethod initialize-instance :after ((obj faderfox-midi) &rest args)
  (with-slots (cc-map cc-nums cc-state note-state chan ff-faders ff-buttons)
      obj
    (setf cc-nums
          (coerce
           (or (getf args :cc-nums)
               '(32 33 34 35 36 37 38 39        ;;; rotaries and button keynums
                 40 41 42 43 44 45 46 47        ;;; 
                 ))
           'vector))
    (dotimes (i 128) (setf (aref cc-map i) nil)) ;;; initialize cc-map with nil

    (loop
      for idx from 0
      for ccnum across cc-nums
      do (setf (aref cc-map ccnum) idx))
    (setf cc-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums) collect (make-instance 'value-cell))))
    (setf note-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums) collect (make-instance 'value-cell))))
    (setf ff-faders cc-state)
    (setf ff-buttons note-state))
  (update-state obj))

(defun midi-delta->i (n)
  (if (zerop (ash n -6))
      n (- n 128)))

;;; (midi-delta->i 126)

(defmethod handle-midi-in ((instance faderfox-midi) opcode d1 d2)
;;;  (call-next-method)
  (with-slots (cc-fns cc-nums ff-fader-update-fns echo
               cc-map cc-state note-state note-fn last-note-on midi-output chan)
      instance
    (case opcode
      (:cc (incudine.util:msg :info "ccin: ~a ~a" d1 d2)
       (cond
         ((< (aref cc-map d1) 16)
          (let* ((fader-idx (aref cc-map d1))
                 (fader-slot (aref cc-state fader-idx))
                 (old-value (val fader-slot))
                 (new-value (max 0 (min 127 (+ old-value (midi-delta->i d2))))))
            (incudine.util:msg :info "old-value: ~a, new-value: ~a" old-value new-value)
            (when (/= old-value new-value)
              (setf (val fader-slot) new-value)
              (when echo (osc-midi-write-short midi-output (+ chan 176) d1 new-value)))))))
      (:note-on (incudine.util:msg :info "notein: ~a ~a" d1 d2)
       (let ((button-idx (aref cc-map d1)))
         (cond ((and (< 3 button-idx 16) (= d2 127))
                (let ((button-slot (aref note-state button-idx)))
                  (toggle-slot button-slot)))
               ((and (< button-idx 3) (= d2 127))
                (let ((button-slot (aref note-state button-idx)))
                  (toggle-slot button-slot)))))))))

(defmethod update-state ((instance faderfox-midi))
  (with-slots (chan cc-nums cc-map cc-state note-state midi-output) instance
    (dotimes (local-idx 16)
      (let ((cc-num (aref cc-nums local-idx)))
        (osc-midi-write-short
         midi-output
         (+ chan 176) cc-num (val (aref cc-state local-idx)))
        ;; (osc-midi-write-short
        ;;  midi-output
        ;;  (+ chan 144) cc-num (val (aref note-state local-idx)))
        ))))

;;; (cellctl:set-ref)
#|
(defmethod (setf s-buttons) (val (obj faderfox) idx)
  (setf (aref (slot-value instance :s-buttons) idx) val)
  val)
|#
