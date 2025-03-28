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
;;          handle-preset-bank-button-press
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
    (loop for idx from 0 for bang across (s-buttons obj) ;;; attach trigger action to top row
          do (push (let ((idx idx)) (lambda () (handle-preset-button-press obj idx))) (trigger-fns bang)))
    (loop for idx from 0 for bang across (m-buttons obj) ;;; attach trigger action to middle row (m-buttons)
          do (push (let ((idx idx)) (lambda () (handle-preset-button-press obj idx))) (trigger-fns bang)))
    (loop for idx from 0 for bang across (r-buttons obj) ;;; attach trigger action to bottom bank row
          do (push (let ((idx idx)) (lambda () (set-val (curr-bank obj) idx))) (trigger-fns bang)))
    (push ;; reaction to val change of curr-bank slot,
                   ;; triggered by pressing any button of r-buttons
                   ;; (see loop above): Relabeling and state update of
                   ;; preset-buttons
              (watch
               (lambda () (let ((bank (get-val curr-bank)))
                       (loop for idx below 8
                             do (if (= idx bank)
                                    (progn
                                      (set-val (aref r-buttons idx) 1)
                                      (loop for i from 0 for label across (subseq button-labels 0 16)
                                            do (set-val label (+ (* bank 16) i)))
                                      (update-preset-buttons obj))
                                    (set-val (aref r-buttons idx) 0))))))
              unwatch)
    (update-hw-state obj)
    (loop for button in (list tr-rewind tr-ffwd tr-stop tr-play) ;;; player-buttons
          do (let ((button button))
               (add-trigger-fn
                button
                (lambda ()
                  (set-val button (if (zerop (get-val button)) 1 0))
                  (update-preset-buttons obj)))))
    (add-trigger-fn tr-rec (lambda () (handle-store-button-press obj)))
    (set-val curr-bank 0)))

(defun update-preset-buttons (controller)
  "React to bank changes, etc. by updating the value of all preset
buttons."
  (let ((preset-offs (* 16 (get-val (curr-bank controller))))
        (active-players (get-active-players controller)))
    (incudine.util:msg :info "update-preset-buttons")
    (dotimes (i 8)
      (set-val (aref (s-buttons controller) i) (preset-state controller (+ preset-offs i) active-players))
      (set-val (aref (m-buttons controller) i) (preset-state controller (+ preset-offs i 8) active-players)))))

(defun handle-preset-button-press (instance button-idx)
  "Handling of button press of s-buttons and m-buttons: Depending on the
state of tr-rec, recall, store or copy a preset."
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
  "Set the player buttons according to /button-state/, a list of 4
numbers with the value 0 or 1."
  (with-slots (tr-rewind tr-ffwd tr-stop tr-play) controller
    (mapc #'set-val
          (list tr-rewind tr-ffwd tr-stop tr-play)
          button-state)))

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

(defun handle-store-button-press (instance)
  (incudine.util:msg :info "store-button-press")
  (set-val (tr-rec instance) (mod (1+ (get-val (tr-rec instance))) 3))
  (when (and (zerop (get-val (tr-rec instance))) (cp-src instance))
    (let* ((src-idx (mod (cp-src instance) 16))
           (slot-name (if (< src-idx 8) 's-buttons 'm-buttons)))
      (set-val (aref (slot-value instance slot-name) (mod src-idx 8)) 0))
    (setf (cp-src instance) nil)
    (update-preset-buttons instance)))

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

#|
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
|#
