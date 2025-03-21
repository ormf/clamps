;;; 
;;; nanoktl-preset-midi.lisp
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

(defparameter *curr-preset* (coerce '(t t t t) 'vector))

(export '(nanoktl2-preset-midi
          *curr-preset*
          get-active-players
          button-labels
          preset-buttons
          preset-state
          pulse
          curr-bank cp-src
          handle-preset-button-press
          handle-preset-bank-button-press
          handle-player-button-press
          handle-store-button-press
          handle-midi-in
          digest-nanoktl2-presets)
        'cl-midictl)


;;; the nanoktl2-preset-midi class extends the nanoktl2-midi class
;;; which contains ref-cells for all buttons with additional slots:
;;;
;;; preset buttons (the top two rows) are special: they have labels
;;; which change dynamically, they have highlight state (0, 1 or 2 for
;;; flashing) and they can be pressed/clicked. Therefore there are 3
;;; slots in nanoktl2-preset-midi to accomodate that:
;;;

;;; 1. s-buttons and m-buttons from the superclass nanoktl2-midi are
;;;    used as ref-cells containing the highlight state and as
;;;    bang-cells triggering actions when pressed.
;;; 2. the additional slots button-labels contain their labels.
;;;
;;; The curr-bank is the current active bank. Pressing a r-button
;;; changes the curr-bank. The curr-bank has a watch function which
;;; highlights the s and m buttons of the nanoctl2-midi superclass
;;; depending on their preset state and the respective r-button.
;;;
;;; The cc-state of the superclass copies the cc-state of *cc-state*
;;; with the cc nums of all hardware elements mapped to the local
;;; indexes 0->45.

(defclass nanoktl2-preset-midi (nanoktl2-midi)
  ((curr-bank :initform (make-ref 0) :initarg :curr-bank :accessor curr-bank
              :documentation "idx of current preset bank")
   (cp-src :initform nil :initarg :cp-src :accessor cp-src
           :documentation "idx of src preset to copy")
   (presets :initform (make-array 128 :initial-contents
                                  (loop repeat 128 collect (make-array 4 :initial-element nil)))
            :initarg :presets :accessor presets)))

(defmethod initialize-instance :after ((obj nanoktl2-preset-midi) &rest args)
  (declare (ignorable args))
  (with-slots (cc-map cc-nums cc-state curr-bank button-labels
               chan midi-output
               nk2-faders s-buttons m-buttons r-buttons unwatch
               tr-rewind tr-ffwd tr-stop tr-play tr-rec)
      obj
    (set-val curr-bank 1)
    (loop for idx from 0 for bang across (s-buttons obj) ;;; attach trigger action to top row
          do (push (let ((idx idx)) (lambda () (handle-preset-button-press obj idx))) (trigger-fns bang)))
    (loop for idx from 0 for bang across (m-buttons obj) ;;; attach trigger action to middle row (m-buttons)
          do (push (let ((idx idx)) (lambda () (handle-preset-button-press obj idx))) (trigger-fns bang)))
    (loop for idx from 0 for bang across (r-buttons obj) ;;; attach trigger action to bottom bank row
          do (push (let ((idx idx)) (lambda () (set-val (curr-bank obj) idx))) (trigger-fns bang)))
    (loop ;;; map ccnum to idx of all nanoktl elems.
      for idx from 0
      for ccnum across cc-nums
      do (setf (aref cc-map ccnum) idx))

    (update-hw-state obj)
    (mapcar #'funcall unwatch)
    (loop for idx below 8
      do (push ;;; bank-change buttons: relabeling of preset-buttons
          (watch
           (let* ((idx idx))
             (lambda () (let ((bank (get-val curr-bank)))
                     (if (= idx bank)
                         (progn
                           (set-val (aref r-buttons idx) 1)
                           (loop for i from 0 for label in button-labels
                                 do (set-val label (+ (* bank 16) i)))
                           (update-preset-buttons obj))
                         (set-val (aref r-buttons idx) 0))))))
          unwatch))
    (let ((opcode (+ (1- chan) 176)))
      (loop for idx below 8 ;;; reflect state change in any of the preset buttons in the hardware LED.
            do (loop
                 for offs in '(16 24)
                 for slot in '(s-buttons m-buttons)
                 do (push
                     (watch
                      (let* ((idx idx)
                             (cc-num (aref cc-nums (+ offs idx)))
                             (button (aref (slot-value obj slot) idx)))
                        (lambda ()
                          (case (get-val button)
                            ((0 1)
                             (osc-midi-write-short
                              midi-output
                              opcode cc-num
                              (if (zerop (get-val button)) 0 127)))
                            (2 (pulse obj button cc-num))))))
                     unwatch)))
      (loop for idx below 8 ;;; reflect preset bank button state change in the hardware LEDs.
            do (push
                (watch
                 (let* ((idx idx)
                        (button (aref r-buttons idx))
                        (cc-num (aref cc-nums (+ 32 idx))))
                   (lambda ()
                     (osc-midi-write-short
                      midi-output
                      opcode cc-num
                      (if (zerop (get-val button)) 0 127)))))
                unwatch))
      (loop for button in (list tr-rewind tr-ffwd tr-stop tr-play) ;;; player-buttons
            for cc-num-idx from 46 
            do (push
                (watch
                 (let* ((button button)
                        (cc-num (aref cc-nums cc-num-idx)))
                   (lambda ()
                     (osc-midi-write-short
                      midi-output
                      opcode cc-num
                      (if (zerop (get-val button)) 0 127))
                     (update-preset-buttons obj))))
                unwatch))
      (push
       (watch
        (let* ((button tr-rec) ;;; store button
               (cc-num (aref cc-nums 50)))
          (lambda ()
            (case (get-val button)
              ((0 1)
               (osc-midi-write-short
                midi-output
                opcode cc-num
                (if (zerop (get-val button)) 0 127)))
              (2 (pulse obj button cc-num))))))
       unwatch))
    (set-val curr-bank 0)
    (set-val (aref r-buttons 0) 1)))

(defun pulse (midi-controller slot cc-num &key (pulse-freq 2) (initial-flash nil))
  "pulse the LED of <midi-controller> at <cc-num> with frequency
<pulse-freq> as long as (get-val <slot>) equals 2. Starting with LED on or
off is determined by <initial-flash>."
  (let ((flash-state initial-flash)
        (pulse-time (/ incudine.util:*sample-rate* pulse-freq 2)))
    (labels ((inner (time)
               (when (= (get-val slot) 2)
                 (let ((next (+ time pulse-time)))
                   (osc-midi-write-short
                    (midi-output midi-controller)
                    (+ (1- (chan midi-controller)) 176) cc-num (if flash-state 127 0))
                   (setf flash-state (not flash-state))
                   (at next #'inner next)))))
      (inner (now)))))

;;; (initialize-instance :after)
(defun update-preset-buttons (controller)
  (let ((preset-offs (* 16 (get-val (curr-bank controller))))
        (active-players (get-active-players controller)))
    (incudine.util:msg :info "update-preset-buttons")
    (dotimes (i 8)
      (set-val (aref (s-buttons controller) i) (preset-state controller (+ preset-offs i) active-players))
      (set-val (aref (m-buttons controller) i) (preset-state controller (+ preset-offs i 8) active-players)))))

(defun handle-preset-button-press (instance button-idx)
  (incudine.util:msg :info "preset-button-press ~a" button-idx)
  (with-slots (curr-bank cp-src tr-rec) instance
    (let ((preset-no (+ (* 16 (get-val curr-bank)) button-idx)))
      (case (get-val tr-rec)
        (0 (unless (zerop (get-val (aref (slot-value instance (if (< button-idx 8) 's-buttons 'm-buttons))
                                         (mod button-idx 8))))
             (recall-preset instance preset-no)
             (update-preset-buttons instance)))
        (1
         (store-preset instance preset-no)
         (set-val tr-rec 0)
         (update-preset-buttons instance))
        (2 (if cp-src ;;; src already seleted? Then copy it to pressed preset button
               (let* ((src-idx (mod cp-src 16))
                      (slot-name (if (< src-idx 8) 's-buttons 'm-buttons))
                      (src-button (aref (slot-value instance slot-name) (mod src-idx 8))))
                 (copy-preset instance cp-src preset-no)
                 (set-val src-button 0) ;;; unhighlight button
                 (setf cp-src nil)
                 (set-val tr-rec 0) ;;; unhighlight store button
                 (update-preset-buttons instance))
               (let* ((slot-name (if (< button-idx 8) 's-buttons 'm-buttons)));;; otherwise flash and set cp-src
                 (set-val (aref (slot-value instance slot-name) (mod button-idx 8)) 2)
                 (setf cp-src preset-no))))))))

(defun set-player-buttons (controller button-state)
  (with-slots (tr-rewind tr-ffwd tr-stop tr-play) controller
    (mapc #'set-val
          (list tr-rewind tr-ffwd tr-stop tr-play)
          button-state)))

#|
(defun handle-preset-bank-button-press (instance button-idx)
  (incudine.util:msg :info "preset-bank-button-press ~a" button-idx)
  (with-slots (curr-bank button-labels r-buttons s-buttons m-buttons) instance
    (set-val (aref r-buttons curr-bank) 0)
    (setf curr-bank button-idx)
    (set-val (aref r-buttons button-idx) 127)
    (loop for i from 0 for label in button-labels
          do (set-val label (+ (* curr-bank 16) i)))
    (update-preset-buttons instance)))
|#

(defun handle-preset-bank-button-press (instance button-idx)
  (incudine.util:msg :info "preset-bank-button-press ~a" button-idx)
  (set-val (curr-bank instance) button-idx))

(defgeneric preset-state (instance preset-no active-players)
  (:method ((instance nanoktl2-preset-midi) preset-no active-players)
    (or
     (block nil
       (dolist (p active-players)
         (when (aref (aref (presets instance) preset-no) p)
           (return 1))))
     0)))

(defun get-active-players (controller)
  (loop for idx below 4
        for slot in '(tr-rewind tr-ffwd tr-stop tr-play)
        unless (zerop (get-val (slot-value controller slot))) collect idx))

(defun handle-player-button-press (instance button-idx)
  (incudine.util:msg :info "player-button-press ~a" button-idx)
  (toggle-slot (slot-value instance (aref #(tr-rewind tr-ffwd tr-stop tr-play) button-idx))))

(defun handle-store-button-press (instance)
  (incudine.util:msg :info "store-button-press")
  (set-val (tr-rec instance) (mod (1+ (get-val (tr-rec instance))) 3))
  (when (and (zerop (get-val (tr-rec instance))) (cp-src instance))
    (let* ((src-idx (mod (cp-src instance) 16))
           (slot-name (if (< src-idx 8) 's-buttons 'm-buttons)))
      (set-val (aref (slot-value instance slot-name) (mod src-idx 8)) 0))
    (setf (cp-src instance) nil)
    (update-preset-buttons instance)))

(defmethod handle-midi-in ((instance nanoktl2-preset-midi) opcode d1 d2)
  (with-slots (cc-fns cc-nums nk2-fader-update-fns
               nk2-fader-modes nk2-fader-last-cc
               hide-fader cc-map cc-state note-fn last-note-on midi-output chan)
      instance
    (incudine.util:msg :info "~a: ~a ~a" opcode d1 d2)
    (case opcode
      (:cc (incudine.util:msg :info "ccin: ~a ~a" d1 d2)
       (let ((fader-idx (aref cc-map d1)))
         (when fader-idx
           (cond
             ((< fader-idx 16) ;;; faders
              (let* ((fn (aref nk2-fader-update-fns fader-idx))
                     (fader-mode (aref nk2-fader-modes fader-idx))
                     (last-cc (aref nk2-fader-last-cc fader-idx))
                     (gui-slot (aref cc-state fader-idx))
                     (val (/ d2 127.0)))
                (case fader-mode
                  (:scale
                   (progn
                     (unless hide-fader
                       (set-val gui-slot (buchla-scale val last-cc (get-val gui-slot) :max 1.0)))
                     (setf (aref nk2-fader-last-cc fader-idx) val)))
                  (:jump (unless hide-fader (set-val gui-slot val)))
                  (:catch (unless hide-fader
                            (if fn
                                (when (funcall fn val (get-val gui-slot))
                                  (set-val gui-slot val)
                                  (setf (aref nk2-fader-update-fns fader-idx) nil))
                                (set-val gui-slot val)))))))
             ((<= 16 fader-idx 31) ;;; s and m buttons
              (when (/= d2 0) (trigger (aref (preset-buttons instance) (- fader-idx 16)))))
             ((<= 32 fader-idx 39) ;;; r buttons
              (when (/= d2 0) (set-val (aref (r-buttons instance) (- fader-idx 32)) 127)))
             ((<= 40 fader-idx 45) ;;; small buttons
              (case fader-idx
                (43 ;;; set button
                 (setf hide-fader (> d2 0)))
                (42 ;;; cycle button
                 (when (/= d2 0) (update-hw-state instance)))
                (otherwise
                 (let ((slot (aref cc-state (aref cc-map d1))))
                   (unless (zerop d2) (trigger slot))))))
             ((<= 46 fader-idx 49) ;;; transport-buttons 1-4
              (when (/= d2 0)
                (handle-player-button-press instance (- fader-idx 46))))
             ((= fader-idx 50) ;;; rec-button
              (when (/= d2 0)
                   (handle-store-button-press instance)))))))
      (:note-on (setf last-note-on d1)))))

(defun select-preset-bank (controller idx)
  (set-val (aref (r-buttons controller) idx) 1))

(defgeneric copy-preset (instance src dest)
  (:method ((controller nanoktl2-preset-midi) src dest)
    (incudine.util:msg :info "copy preset from ~a to ~a~%" src dest)
    (unless (= src dest)
      (with-slots (presets) controller
        (loop
          for player in (get-active-players controller)
          do (setf (aref (aref presets player) dest)
;;;                   (copy-structure (aref (aref presets player) src))
                   (aref (aref presets player) src)))))))

(defgeneric store-preset (instance dest)
  (:method ((controller nanoktl2-preset-midi) dest)
    (with-slots (presets) controller
      (incudine.util:msg :info "store preset to ~a~%" dest)
      (loop
        for player in (get-active-players controller)
        do (setf (aref (aref presets player) dest)
                 ;;;(copy-structure (aref *curr-preset* player))
                 (aref *curr-preset* player))))))

(defgeneric recall-preset (instance src)
  (:method ((controller nanoktl2-preset-midi) src)
    (incudine.util:msg :info "recall preset from ~a~%" src)
    (with-slots (presets) controller
      (loop
        for player in (get-active-players controller)
        do (setf (aref *curr-preset* player)
;;;                 (copy-structure (aref (aref presets player) src))
                 (aref (aref presets player) src))))))

(defvar *nanoktl2-presets-file* "/tmp/nanoktl2-presets.lisp")
(defvar *curr-nk2-controller* nil)

#|

(defun format-player-preset (preset stream)
  (declare (ignorable stream))
;;;  (format stream "~&(")
  (if preset
      (format stream "~&(:num ~a :cc-state ~a~%:form ~a)~%"
              (f.orm::controller-preset-num preset)
              (f.orm::controller-preset-cc-state preset)
              (aref (f.orm::controller-preset-fns preset) 0) )
      (format stream "nil "))
;;;  (format stream ")~%")
  )
|#
(defun format-player-preset (preset stream)
  (declare (ignorable stream))
;;;  (format stream "~&(")
  (if preset
      (format stream preset)
      (format stream "nil "))
;;;  (format stream ")~%")
  )

(defgeneric save-presets (instance &optional file)
  (:method ((controller nanoktl2-preset-midi) &optional (file *nanoktl2-presets-file*))
    (incudine.util:msg :info "saving nanoktl2 preset to ~S~%" file)
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "(in-package :f.orm)~%~%(digest-nanoktl2-presets cl-midictl::*curr-nk2-controller*~%'(")
      (map nil
           (lambda (preset)
             (format out "(")
             (map nil
                  (lambda (player-preset) (format-player-preset player-preset out))
                  preset)
             (format out ")~%"))
           (presets controller))
      (format out "))"))
    file))

(defun digest-nanoktl2-presets (controller data)
  (declare (ignorable data))
  (format t "digesting presets from: ~a ~%" controller))

(defgeneric load-presets (instance &optional file)
    (:method ((controller nanoktl2-preset-midi) &optional (file *nanoktl2-presets-file*))
    (incudine.util:msg :warn "loading nanoktl2 preset from ~S~%" file)
      (let ((*curr-nk2-controller* controller))
        (load file)
        (update-preset-buttons controller))))

(defgeneric init-nk2 (instance &optional file)
  (:method ((controller nanoktl2-preset-midi) &optional (file *nanoktl2-presets-file*))
    (set-player-buttons controller '(1 1 1 1))
    (load-presets controller file)
    (handle-preset-button-press controller 0)))
