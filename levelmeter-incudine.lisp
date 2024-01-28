;;;; levelmeter-incudine.lisp
;;;;
;;;; Copyright (c) 2018-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:incudine)
(export 'meters-dsp :incudine)

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
                (/ (- 1 (cos (* 2 pi (/ i (1- size))))) size)))
        (values c-array nil nil)))))

(define-vug levelmeter (in freq (ref cl-refs:ref-object))
  (with ((sum 0)
         (count 0)
         (max (round-sample (/ *sample-rate* freq)))
         (value 0))
    (declare (sample sum) (fixnum count max value))
    (prog1 count
      (incf count)
      (incf sum (* in in))
      (when (>= count max)
        (setf value
              (round-sample
                (+ 100 (linear->db
                         (sqrt (the non-negative-sample (/ sum count)))))))
        (nrt-funcall
          (lambda ()
            (cl-refs:set-val ref value)))
        (setf sum +sample-zero+)
        (setf count 0)))))

(declaim (inline env-levelmeter))
(define-vug env-levelmeter (in (freq fixnum) (ref cl-refs:ref-object)
                               (periods channel-number))
     (:defaults +sample-zero+ 10 nil 2)
     (with ((size (round-sample (/ (* periods *sample-rate*) freq)))
            (hanning (make-buffer (1+ size) :fill-function (hanning-rms)))
            (sums (make-frame periods :zero-p t))
            (bufidx (make-array periods :element-type 'channel-number))
            (max +sample-zero+)
            (value 0)
            (last-value #.most-positive-fixnum))
       (declare  (fixnum size value last-value) (sample max))
       (initialize
        (dotimes (i periods)
          (setf (aref bufidx i)
                (reduce-warnings (floor (* i (round (/ size periods))))))))
       (dotimes (i periods)
         (incf (smp-ref sums i) (* in in (buffer-value hanning (aref bufidx i))))
         (when (>= (incf (aref bufidx i)) size)
           (setf value
                 (round-sample
                  (+ 100 (linear->db
                          (sqrt (the non-negative-sample (smp-ref sums i)))))))
           (if (/= (cl-refs:get-val ref) value)
               (progn
                 (nrt-funcall
                  (lambda ()
                    (cl-refs:set-val ref (max 0 value))))
                 (setf last-value value)))
           (Setf (smp-ref sums i) +sample-zero+)
           (setf (aref bufidx i) 0)))))

(dsp! env-monometer ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
   (foreach-frame (env-levelmeter (audio-in chan) freq ref hop-size)))

;;; (defparameter incudine::*note-ids* '(1 2 3))

(defun meters-dsp (&key (group 300) (num *number-of-input-bus-channels*)
                     refs (freq 5) (hop-size 2) (audio-bus 0))
  (sleep 1)
  (dotimes (idx num)
    (env-monometer freq (aref refs idx)
                   (+ audio-bus idx) hop-size
                   ;; :action (lambda (n)
                   ;;           (format t "~&pushing: ~a~%" (node-id n))
                   ;;           (push (node-id n)
                   ;;                 incudine::*node-ids*))
                   :tail group)))

