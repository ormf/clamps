;;;
;;; levelmeter-incudine.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package #:incudine)
(export '(meters-dsp inmeters-dsp outmeters-dsp) :incudine)

(declaim (inline round-sample))
(defun round-sample (x)
  (declare (type (sample
                  #.(coerce (ash most-negative-fixnum -1) 'sample)
                  #.(coerce (ash most-positive-fixnum -1) 'sample))
                 x))
  (multiple-value-bind (result rem) (round x)
    (declare (ignore rem))
    result))

(defun hanning ()
  (lambda (c-array size)
    (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (* 0.5 (- 1 (cos (* 2 pi (/ i (1- size))))))))
        (values c-array nil nil)))))

(defun hanning-rms ()
  "(/ size) weighted version of (* 2 hanning)"
  (lambda (c-array size)
    (declare (type foreign-pointer c-array) (type non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (/ (- 1 (cos (* 2 pi (/ i (1- size))))) (1- size))))
        (values c-array nil nil)))))

(declaim (inline env-inmeter))
(define-vug env-inmeter ((in channel-number) (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (bufidx (make-array periods :element-type 'channel-number
                             :initial-contents (loop for i below periods collect (floor (* i (round (/ size periods)))))))
         (max +sample-zero+)
         (value 0)
         (last-value #.most-positive-fixnum)
         )
    (declare  (fixnum size value last-value) (sample max))
    (dotimes (i periods)
      (let ((buf-idx (round-sample (%phasor 1 (aref bufidx i) size))))
        (incf (smp-ref sums i) (* (audio-in in) (audio-in in) (buffer-value hanning buf-idx)))
        (when (zerop buf-idx)
          (progn
            (setf value
                  (round-sample
                   (+ 100 (linear->db
                           (sqrt (the non-negative-sample (smp-ref sums i)))))))
            (if (/= last-value value)
                (progn
                  (nrt-funcall
                   (lambda ()
                     (cl-refs:set-val ref value)))
                  (setf last-value value)))
            (setf (smp-ref sums i) +sample-zero+)))))))

(declaim (inline env-outmeter))
(define-vug env-outmeter ((out channel-number) (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (bufidx (make-array periods :element-type 'channel-number
                             :initial-contents (loop for i below periods collect (floor (* i (round (/ size periods)))))))
         (max +sample-zero+)
         (value 0)
         (last-value #.most-positive-fixnum)
         )
    (declare  (fixnum size value last-value) (sample max))
    (dotimes (i periods)
      (let ((buf-idx (round-sample (%phasor 1 (aref bufidx i) size))))
        (incf (smp-ref sums i) (* (audio-out out) (audio-out out) (buffer-value hanning buf-idx)))
        (when (zerop buf-idx)
          (progn
            (setf value
                  (round-sample
                   (+ 100 (linear->db
                           (sqrt (the non-negative-sample (smp-ref sums i)))))))
            (if (/= last-value value)
                (progn
                  (nrt-funcall
                   (lambda ()
                     (cl-refs:set-val ref value)))
                  (setf last-value value)))
            (setf (smp-ref sums i) +sample-zero+)))))))

(declaim (inline env-levelmeter))
(define-vug env-levelmeter (in (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (bufidx (make-array periods :element-type 'channel-number
                             :initial-contents (loop for i below periods collect (floor (* i (round (/ size periods)))))))
         (max +sample-zero+)
         (value 0)
         (last-value #.most-positive-fixnum)
         )
    (declare  (fixnum size value last-value) (sample max))
    (dotimes (i periods)
      (let ((buf-idx (round-sample (%phasor 1 (aref bufidx i) size))))
        (incf (smp-ref sums i) (* in in (buffer-value hanning buf-idx)))
        (when (zerop buf-idx)
          (progn
            (setf value
                  (round-sample
                   (+ 100 (linear->db
                           (sqrt (the non-negative-sample (smp-ref sums i)))))))
            (if (/= last-value value)
                (progn
                  (nrt-funcall
                   (lambda ()
                     (cl-refs:set-val ref value)))
                  (setf last-value value)))
            (setf (smp-ref sums i) +sample-zero+)))))))

(dsp! env-monometer ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-levelmeter (bus chan) freq ref hop-size)))

(dsp! env-monoinmeter ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-inmeter chan freq ref hop-size)))

(dsp! env-monooutmeter ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-outmeter chan freq ref hop-size)))

(defparameter *node-ids* '())

(export *node-ids* :incudine)

(defun meters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0))
  (loop
    for idx below num
    do (progn
         (env-monometer freq (aref refs idx)
                        (+ audio-bus idx) hop-size
                        :action (lambda (n)
                                  (funcall id-callback (node-id n)))
                        :tail group))))

(defun inmeters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0))
  (loop
    for idx below num
    do (progn
         (env-monoinmeter freq (aref refs idx)
                        (+ audio-bus idx) hop-size
                        :action (lambda (n)
                                  (funcall id-callback (node-id n)))
                        :tail group))))

(defun outmeters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0))
  (loop
    for idx below num
    do (progn
         (env-monooutmeter freq (aref refs idx)
                        (+ audio-bus idx) hop-size
                        :action (lambda (n)
                                  (funcall id-callback (node-id n)))
                        :tail group))))
