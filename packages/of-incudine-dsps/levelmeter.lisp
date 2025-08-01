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
(export '(meters-dsp inmeters-dsp outmeters-dsp master-amp-meter-bus-dsp
          master-amp-meter-out-dsp)
        :incudine)

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
  (:defaults 0 10 nil 2)
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

#|
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
|#

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

(declaim (inline env-levelmultiinmeter))
(define-vug env-levelmultiinmeter ((in channel-number) (num channel-number)
                                   (freq fixnum) (refs (simple-array cl-refs:ref-object))
                                   (periods channel-number))
  (:defaults 0 1 10 nil 2)
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
        (dochannels (i periods)            
          (dochannels (ch num)
            (incf (smp-ref sums (+ (* ch periods) i))
                  (* (audio-in (+ in ch) current-frame) (audio-in (+ in ch) current-frame)
                     (the sample (buffer-value hanning (aref phases i)))))
            (reduce-warnings
              (when (zerop ch) (incf (aref phases i)))
              (when (>=  (aref phases i) size)
                (when (= ch (1- num)) (setf (aref phases i) 0))
                (setf (aref value ch)
                      (max -100 (round (power->db (smp-ref sums (+ (* ch periods) i))))))
                (when (/= (aref last-value ch) (aref value ch))
                  (nrt-funcall
                   (let*
                       ((ch ch) (i i) (value (aref value ch))
                        (ref (aref refs ch)))
                     (lambda ()
                       (cl-refs:set-val ref value))))
                  (setf (aref last-value ch) (aref value ch)))
                (setf (smp-ref sums (+ (* ch periods) i)) +sample-zero+)))))))))

(dsp! env-multiinmeter ((freq fixnum) (num-channels channel-number)
                        (refs (simple-array cl-refs:ref-object))
                        (chan channel-number)
                        (hop-size channel-number))
   (:defaults 10 1 nil 0 2)
  (foreach-frame (env-levelmultiinmeter chan num-channels freq refs hop-size)))

(declaim (inline env-levelmultioutmeter))
(define-vug env-levelmultioutmeter ((in channel-number) (num channel-number)
                                   (freq fixnum) (refs (simple-array cl-refs:ref-object))
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
        (dochannels (i periods)            
          (dochannels (ch num)
            (incf (smp-ref sums (+ (* ch periods) i))
                  (* (audio-out (+ in ch) current-frame) (audio-out (+ in ch) current-frame)
                     (the sample (buffer-value hanning (aref phases i)))))
            (reduce-warnings
              (when (zerop ch) (incf (aref phases i)))
              (when (>=  (aref phases i) size)
                (when (= ch (1- num)) (setf (aref phases i) 0))
                (setf (aref value ch)
                      (max -100 (round (power->db (smp-ref sums (+ (* ch periods) i))))))
                (when (/= (aref last-value ch) (aref value ch))
                  (nrt-funcall
                   (let*
                       ((ch ch) (i i) (value (aref value ch))
                        (ref (aref refs ch)))
                     (lambda ()
                       (cl-refs:set-val ref value))))
                  (setf (aref last-value ch) (aref value ch)))
                (setf (smp-ref sums (+ (* ch periods) i)) +sample-zero+)))))))))

(dsp! env-multioutmeter ((freq fixnum) (num channel-number)
                         (refs (simple-array cl-refs:ref-object))
                         (chan channel-number)
                         (hop-size channel-number))
   (:defaults 10 1 nil 0 2)
  (foreach-frame (env-levelmultioutmeter chan num freq refs hop-size)))

(declaim (inline env-levelmultimeter))
(define-vug env-levelmultimeter ((in channel-number) (num channel-number)
                                   (freq fixnum) (refs (simple-array cl-refs:ref-object))
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
        (dochannels (i periods)            
          (dochannels (ch num)
            (with-samples ((val (audio-bus (+ in ch) current-frame)))
              (incf (smp-ref sums (+ (* ch periods) i))
                    (* val val
                       (the sample (buffer-value hanning (aref phases i))))))
            (reduce-warnings
              (when (zerop ch) (incf (aref phases i)))
              (when (>=  (aref phases i) size)
                (when (= ch (1- num)) (setf (aref phases i) 0))
                (setf (aref value ch)
                      (max -100 (round (power->db (smp-ref sums (+ (* ch periods) i))))))
                (when (/= (aref last-value ch) (aref value ch))
                  (nrt-funcall
                   (let*
                       ((ch ch) (i i) (value (aref value ch))
                        (ref (aref refs ch)))
                     (lambda ()
                       (cl-refs:set-val ref value))))
                  (setf (aref last-value ch) (aref value ch)))
                (setf (smp-ref sums (+ (* ch periods) i)) +sample-zero+)))))))))

(dsp! env-multimeter ((freq fixnum) (num channel-number)
                      (refs (simple-array cl-refs:ref-object))
                      (chan channel-number)
                      (hop-size channel-number))
   (:defaults 10 1 nil 0 2)
  (foreach-frame (env-levelmultimeter chan num freq refs hop-size)))

(declaim (inline master-amp-meter-bus-vug))
(define-vug master-amp-meter-bus-vug ((in channel-number) (out channel-number)
                                      (num channel-number)
                                      amp
                                      (freq alexandria:positive-fixnum)
                                      (meter-refs (simple-array cl-refs:ref-object))
                                      (pre boolean)
                                      (periods channel-number))
  (:defaults +sample-zero+ 0 1 1 10 nil nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame (* num periods) :zero-p t))
         (phases (reduce-warnings
                   (make-array periods
                               :element-type 'alexandria:non-negative-fixnum
                               :initial-contents
                               (loop for i below periods
                                     collect (round (* (/ i periods) size))))))
         (value (make-array num :initial-element 0))
         (last-value (make-array num :initial-element #.most-positive-fixnum)))
    (declare (type alexandria:non-negative-fixnum size))
    (reduce-warnings
      (with ((lag-amp (lag amp 0.05)))
;;;        (declare (sample lag-amp))
        (foreach-frame
          (dochannels (ch num)
            (with-samples ((val (* lag-amp (audio-bus (+ in ch) current-frame))))
              (incf (audio-out ch) val)
              (dochannels (i periods)            
                (incf (smp-ref sums (+ (* ch periods) i))
                      (* val val
                         (the sample (buffer-value hanning (aref phases i)))))
                (when (zerop ch) (incf (aref phases i)))
                (when (>=  (aref phases i) size)
                  (when (= ch (1- num)) (setf (aref phases i) 0))
                  (setf (aref value ch)
                        (max -100 (round (power->db (smp-ref sums (+ (* ch periods) i))))))
                  (when (/= (aref last-value ch) (aref value ch))
                    (nrt-funcall
                     (let*
                         ((ch ch) (value (aref value ch))
                          (ref (aref meter-refs ch)))
                       (lambda ()
                         (cl-refs:set-val ref value))))
                    (setf (aref last-value ch) (aref value ch)))
                  (setf (smp-ref sums (+ (* ch periods) i)) +sample-zero+))))
            ;; clearing the audio-bus for the next dsp graph cycle:
            (setf (audio-bus (+ in ch) current-frame) +sample-zero+)))))))

(dsp! master-amp-meter-bus ((bus-num channel-number)
                            (out channel-number)
                            (num-channels channel-number)
                            amp
                            (freq alexandria:positive-fixnum)
                            (meter-refs (simple-array cl-refs:ref-object))
                            (pre boolean)
                            (hop-size channel-number))

   (:defaults 0 0 1 1 10 nil nil 2)
  (foreach-frame (master-amp-meter-bus-vug bus-num out num-channels amp freq meter-refs pre hop-size)))

(declaim (inline master-amp-meter-out-vug))
(define-vug master-amp-meter-out-vug ((out channel-number)
                                      (num channel-number)
                                      amp
                                      (freq alexandria:positive-fixnum)
                                      (meter-refs (simple-array cl-refs:ref-object))
                                      (pre boolean)
                                      (periods channel-number))
  (:defaults 0 1 1 10 nil nil 2)
  (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
         (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
         (sums (make-frame (* num periods) :zero-p t))
         (phases (reduce-warnings
                   (make-array periods
                               :element-type 'alexandria:non-negative-fixnum
                               :initial-contents
                               (loop for i below periods
                                     collect (round (* (/ i periods) size))))))
         (value (make-array num :initial-element 0))
         (last-value (make-array num :initial-element #.most-positive-fixnum)))
    (declare (type alexandria:non-negative-fixnum size))
    (reduce-warnings
      (with ((lag-amp (lag amp 0.05)))
;;;        (declare (sample lag-amp))
        (foreach-frame
          (dochannels (ch num)
            (with-samples ((val (* lag-amp (audio-out (+ out ch) current-frame))))
              (setf (audio-out (+ out ch)) val)
              (dochannels (i periods)            
                (incf (smp-ref sums (+ (* ch periods) i))
                      (* val val
                         (the sample (buffer-value hanning (aref phases i)))))
                (when (zerop ch) (incf (aref phases i)))
                (when (>=  (aref phases i) size)
                  (when (= ch (1- num)) (setf (aref phases i) 0))
                  (setf (aref value ch)
                        (max -100 (round (power->db (smp-ref sums (+ (* ch periods) i))))))
                  (when (/= (aref last-value ch) (aref value ch))
                    (nrt-funcall
                     (let*
                         ((ch ch) (value (aref value ch))
                          (ref (aref meter-refs ch)))
                       (lambda ()
                         (cl-refs:set-val ref value))))
                    (setf (aref last-value ch) (aref value ch)))
                  (setf (smp-ref sums (+ (* ch periods) i)) +sample-zero+))))))))))

(dsp! master-amp-meter-out ((out channel-number)
                            (num-channels channel-number)
                            amp
                            (freq alexandria:positive-fixnum)
                            (meter-refs (simple-array cl-refs:ref-object))
                            (pre boolean)
                            (hop-size channel-number))

   (:defaults 0 1 1 10 nil nil 2)
  (foreach-frame (master-amp-meter-out-vug out num-channels amp freq meter-refs pre hop-size)))

(defun master-amp-meter-bus-dsp (&key (group 300) id-callback (bus-num 0) (audio-out 0) (num-channels 1) (amp 1) (freq 10) meter-refs pre (hop-size 2))
  "wrapper around master-amp-meter-bus with a callback to register the
node id after instantiation."
  (master-amp-meter-bus bus-num audio-out num-channels amp freq meter-refs pre hop-size
                 :action (lambda (n)
                           (funcall id-callback (node-id n)))
                 :tail group))

(defun master-amp-meter-out-dsp (&key (group 300) id-callback (audio-out 0) (num-channels 1) (amp 1) (freq 10) meter-refs pre (hop-size 2))
  "wrapper around master-amp-meter-bus with a callback to register the
node id after instantiation."
  (master-amp-meter-out audio-out num-channels amp freq meter-refs pre hop-size
                 :action (lambda (n)
                           (funcall id-callback (node-id n)))
                 :tail group))

#|

;;; deprecated: creating num monometers

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

(defun inmeters-dsp (&key (group 300) (num *number-of-output-bus-channels*)
                        id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                        after before)
  (dotimes (idx num)
    (apply #'env-monoinmeter freq (aref refs idx)
           (+ audio-bus idx) hop-size
           :action (lambda (n)
                     (funcall id-callback (node-id n)))
           :tail group
           (cond (after `(:after ,after))
                 (before `(:before ,before))))))

(defun outmeters-dsp (&key (group 300) (num *number-of-output-bus-channels*)
                        id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                        after before)
  (dotimes (idx num)
    (apply #'env-monooutmeter freq (aref refs idx)
           (+ audio-bus idx) hop-size
           :action (lambda (n)
                     (funcall id-callback (node-id n)))
           :tail group
           (cond (after `(:after ,after))
                 (before `(:before ,before))))))

|#


(defun meters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                     after before)
  (apply #'env-multimeter
         freq num refs
         audio-bus hop-size
         :action (lambda (n)
                   (funcall id-callback (node-id n)))
         :tail group
         (cond (after `(:after ,after))
               (before `(:before ,before)))))

(defun inmeters-dsp (&key (group 100) (num *number-of-input-bus-channels*)
                       id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                       after before)
   (apply #'env-multiinmeter freq num refs
          audio-bus hop-size
          :action (lambda (n)
                    (funcall id-callback (node-id n)))
          :tail group
          (cond (after `(:after ,after))
                (before `(:before ,before)))))

(defun outmeters-dsp (&key (group 100) (num *number-of-input-bus-channels*)
                       id-callback refs (freq 5) (hop-size 2) (audio-bus 0)
                       after before)
   (apply #'env-multioutmeter freq num refs
          audio-bus hop-size
          :action (lambda (n)
                    (funcall id-callback (node-id n)))
          :tail group
          (cond (after `(:after ,after))
                (before `(:before ,before)))))
