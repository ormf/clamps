;;;
;;; levelmeter.lisp
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
    (declare (type foreign-pointer c-array) (type alexandria:non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (* 0.5 (- 1 (cos (* 2 pi (/ i (1- size))))))))
        (values c-array nil nil)))))

(defun hanning-rms ()
  "(/ size) weighted version of (* 2 hanning)"
  (lambda (c-array size)
    (declare (type foreign-pointer c-array) (type alexandria:non-negative-fixnum size))
    (with-foreign-array (tmp 'sample)
      (with-samples (value abs-value (max 0.0))
        (dotimes (i size)
          (setf (smp-ref c-array i)
                (/ (- 1 (cos (* 2 pi (/ i (1- size))))) (1- size))))
        (values c-array nil nil)))))

#|
(deftype alexandria:non-negative-fixnum ()
      `(integer 0 , most-positive-fixnum))
|#

(defun power->db (value)
  "Convert the VALUE from linear to dB."
  (let ((in (if (zerop value) least-positive-sample value)))
    (* (log in) #.(/ (sample 10) (log (sample 10))))))

(declaim (inline env-inmeter))
(define-vug env-inmeter ((in channel-number) (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (phase-offs (make-array periods :element-type 'alexandria:non-negative-fixnum :initial-contents
                                 (loop for i below periods
                                       collect (round (* (/ i periods) size)))))
         (phases (make-array periods :element-type 'alexandria:non-negative-fixnum
                                     :initial-contents
                                     (loop for i below periods
                                           collect (round (* (/ i periods) size)))))
         (value 0)
         (last-value #.most-positive-fixnum))
    (declare (alexandria:non-negative-fixnum size value last-value))
    (dotimes (i periods)
      (incf (smp-ref sums i)
            (* (audio-in in) (audio-in in)
               (the sample (buffer-value hanning (aref phases i)))))
      (when (>= (incf (aref phases i)) size)
        (setf (aref phases i) 0)
        (setf value (reduce-warnings (+ 100 (max -100 (round (power->db (smp-ref sums i)))))))
        (if (/= last-value value)
            (progn
              (nrt-funcall
               (lambda ()
                 (cl-refs:set-val ref (- value 100))))
              (setf last-value value)))
        (setf (smp-ref sums i) +sample-zero+)))))

(dsp! env-monoinmeter ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-inmeter chan freq ref hop-size)))

(defun inmeters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0))
  (loop
    for idx below num
    do (progn
         (incudine.util:msg :warn "inmeter: ~a" idx)
         (env-monoinmeter freq (aref refs idx)
                        (+ audio-bus idx) hop-size
                        :action (lambda (n)
                                  (funcall id-callback (node-id n)))
                        :tail group))))

(declaim (inline env-outmeter))
(define-vug env-outmeter ((out channel-number) (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (phase-offs (make-array periods :element-type 'alexandria:non-negative-fixnum :initial-contents
                                 (loop for i below periods
                                       collect (round (* (/ i periods) size)))))
         (phases (make-array periods :element-type 'alexandria:non-negative-fixnum
                                     :initial-contents
                                     (loop for i below periods
                                           collect (round (* (/ i periods) size)))))
         (value 0)
         (last-value #.most-positive-fixnum))
    (declare (alexandria:non-negative-fixnum size value last-value))
    (dotimes (i periods)
      (incf (smp-ref sums i)
            (* (audio-out out) (audio-out out)
               (the sample (buffer-value hanning (aref phases i)))))
      (when (>= (incf (aref phases i)) size)
        (setf (aref phases i) 0)
        (setf value (reduce-warnings (+ 100 (max -100 (round (power->db (smp-ref sums i)))))))
        (if (/= last-value value)
            (progn
              (nrt-funcall
               (lambda ()
                 (cl-refs:set-val ref (- value 100))))
              (setf last-value value)))
        (setf (smp-ref sums i) +sample-zero+)))))

(dsp! env-monooutmeter ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-outmeter chan freq ref hop-size)))

(declaim (inline env-levelmeter))
(define-vug env-levelmeter ((in channel-number) (freq fixnum) (ref cl-refs:ref-object)
                              (periods channel-number))
  (:defaults +sample-zero+ 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame periods :zero-p t))
         (phases (make-array periods :element-type 'alexandria:non-negative-fixnum
                                     :initial-contents
                                     (loop for i below periods
                                           collect (round (* (/ i periods) size)))))
         (value 0)
         (last-value #.most-positive-fixnum))
    (declare (alexandria:non-negative-fixnum size value last-value))
    (reduce-warnings
      (foreach-frame
        (dotimes (i periods)
          (incf (smp-ref sums i)
                (* (the sample (audio-bus in current-frame))
                   (the sample (audio-bus in current-frame))
                   (the sample (buffer-value hanning (aref phases i)))))
          (when (>= (incf (aref phases i)) size)
            (setf (aref phases i) 0)
            (setf value (reduce-warnings (+ 100 (max -100 (round (power->db (smp-ref sums i)))))))
            (when (/= last-value value)
              (progn
                (nrt-funcall
                 (lambda ()
                   (cl-refs:set-val ref (- value 100))))
                (setf last-value value)))
            (setf (smp-ref sums i) +sample-zero+)))))))

(dsp! env-monometer ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
  (foreach-frame (env-levelmeter chan freq ref hop-size)))

(declaim (inline env-levelmultimeter))
(define-vug env-levelmultimeter ((in channel-number) (num channel-number)
                                 (freq fixnum) (ref (simple-array cl-refs:ref-object))
                                 (periods channel-number))
  (:defaults +sample-zero+ 1 10 nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame (* num periods) :zero-p t))
         (phases (make-array periods
                             :element-type 'alexandria:non-negative-fixnum
                             :initial-contents
                             (loop for i below periods
                                   collect (round (* (/ i periods) size)))))
         (value (make-array num :initial-element 0))
         (last-value (make-array num :initial-element #.most-positive-fixnum)))
    (declare (type alexandria:non-negative-fixnum size))
    (reduce-warnings
      (foreach-frame
        (dotimes (ch num)
          (dotimes (i periods)
            (incf (smp-ref sums (+ ch (* i num)))
                  (* (the sample (audio-bus (+ in ch) current-frame))
                     (the sample (audio-bus (+ in ch) current-frame))
                     (the sample (buffer-value hanning (aref phases i)))))
            (reduce-warnings
              (when (>= (incf (aref phases i)) size)
                (setf (aref phases i) 0)
                (setf (aref value ch) (reduce-warnings
                                        (+ 100 (max -100 (round (power->db (smp-ref sums (+ ch (* i num)))))))))
                (when (/= (aref last-value ch) (aref value ch))
                  (progn
                    (nrt-funcall
                     (lambda ()
                       (cl-refs:set-val (aref ref ch) (- (aref value ch) 100))))
                    (setf (aref last-value ch) (aref value ch))))
                (setf (smp-ref sums (+ ch (* i num))) +sample-zero+)))))))))

(dsp! env-multimeter ((freq fixnum) (num channel-number)
                      (ref (simple-array cl-refs:ref-object))
                      (chan channel-number)
                      (hop-size channel-number))
   (:defaults 10 1 nil 0 2)
  (foreach-frame (env-levelmultimeter chan num freq ref hop-size)))


(defparameter *node-ids* '())

(export *node-ids* :incudine)

(defun meters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                     after before)
  (loop
    for idx below num
    do (progn
         (apply #'env-monometer
                freq (aref refs idx)
                (+ audio-bus idx) hop-size
                :action (lambda (n)
                          (funcall id-callback (node-id n)))
                :tail group
                (cond (after `(:after ,after))
                      (before `(:before ,before)))))))

(defun inmeters-dsp (&key (group 100) (num *number-of-input-bus-channels*)
                       id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                       after before)
  (loop
    for idx below num
    do (progn
         (apply #'env-monoinmeter freq (aref refs idx)
                (+ audio-bus idx) hop-size
                :action (lambda (n)
                          (funcall id-callback (node-id n)))
                :tail group
                (cond (after `(:after ,after))
                      (before `(:before ,before)))))))

(defun outmeters-dsp (&key (group 300) (num *number-of-output-bus-channels*)
                        id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                        after before)
  (loop
    for idx below num
    do (progn
         (apply #'env-monooutmeter freq (aref refs idx)
                (+ audio-bus idx) hop-size
                :action (lambda (n)
                          (funcall id-callback (node-id n)))
                :tail group
                (cond (after `(:after ,after))
                      (before `(:before ,before)))))))
