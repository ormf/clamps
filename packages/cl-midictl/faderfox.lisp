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
  ((cc-nums :accessor cc-nums)))

;;; the cc-map maps the cc nums to the array slots. Their order is like this:
#|
array-idx: cc-nums of nanokontrol2

32-47:     cc-nums of faders
32-47:     keynums of buttons

the cc-map below is according to the factory settings of a
nanokontrol2.

|#

(defmethod initialize-instance :after ((obj faderfox-midi) &rest args)
  (with-slots (cc-map cc-nums cc-state note-state chan ff-faders ff-buttons
               midi-output unwatch)
      obj
    (setf cc-nums
          (coerce
           (or (getf args :cc-nums)
               '(32 33 34 35   ;;; rotary ccnums and button keynums
                 36 37 38 39        
                 40 41 42 43
                 44 45 46 47))
           'vector))
    (dotimes (i 128) (setf (aref cc-map i) nil)) ;;; initialize cc-map with nil
    (loop
      for idx from 0
      for ccnum across cc-nums
      do (setf (aref cc-map ccnum) idx))
    (setf cc-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums) collect (make-ref 0))))
    (setf note-state (make-array (length cc-nums)
                               :initial-contents
                               (loop for x below (length cc-nums) collect (make-ref 0))))
    (dotimes (idx 16)
      (push
       (watch
        (let* ((idx idx)
               (cc-num (aref cc-nums idx))
               (opcode (+ 176 (1- chan)))
               (cc-state (aref cc-state idx)))
          (lambda () (osc-midi-write-short midi-output opcode cc-num (round (* 127 (get-val cc-state)))))))
       unwatch)))
  (update-hw-state obj))

(defun midi-delta->i (n)
  (if (zerop (ash n -6))
      n (- n 128)))

;;; (midi-delta->i 126)

(defmethod handle-midi-in ((instance faderfox-midi) opcode channel d1 d2)
  (with-slots (cc-fns cc-nums ff-fader-update-fns echo
               cc-map cc-state note-state note-fn last-note-on midi-output chan)
      instance
    (incudine.util:msg :debug "faderfox ~S ~a ~a" opcode d1 d2)
    (if (= chan (1+ channel))
        (case opcode
          (:cc
           (cond
             ((< (aref cc-map d1) 16)
              (let* ((fader-idx (aref cc-map d1))
                     (fader-slot (aref cc-state fader-idx))
                     (old-value (get-val fader-slot))
                     (new-value (max 0 (min 1 (float (+ old-value (/ (midi-delta->i d2) 127)) 1.0)))))
                (incudine.util:msg :debug "old-value: ~a, new-value: ~a" old-value new-value)
                (when (/= old-value new-value)
                  (set-val fader-slot new-value))))))
          (:note-on
           (let ((button-idx (aref cc-map d1)))
             (incudine.util:msg :debug "button-idx ~a" button-idx)
             (let ((button-slot (aref note-state button-idx)))
               (toggle-slot button-slot))))))))

(defmethod update-hw-state ((instance faderfox-midi))
  (with-slots (chan cc-nums cc-map cc-state note-state midi-output) instance
    (dotimes (local-idx 16)
      (let ((cc-num (aref cc-nums local-idx)))
        (osc-midi-write-short
         midi-output
         (+ (1- chan) 176) cc-num (round (* 127 (get-val (aref cc-state local-idx)))))
        ;; (osc-midi-write-short
        ;;  midi-output
        ;;  (+ (1- chan) 144) cc-num (get-val (aref note-state local-idx)))
        ))))

;;; (cellctl:set-ref)
#|
(defmethod (setf s-buttons) (get-val (obj faderfox) idx)
  (setf (aref (slot-value instance :s-buttons) idx) val)
  val)
|#
