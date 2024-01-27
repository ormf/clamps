;;;; levelmeter-incudine.lisp
;;;;
;;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(in-package #:scratch)
(export 'meters :scratch)

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
             (Setf (smp-ref sums i) +sample-zero+)
             (setf (aref bufidx i) 0))))))

(dsp! env-monometer ((freq fixnum) (ref cl-refs:ref-object) (chan channel-number)
                     (hop-size channel-number))
   (:defaults 10 nil 0 2)
   (foreach-frame (env-levelmeter (bus chan) freq ref hop-size)))
 
(defun meters (&key (group 300) (num *number-of-input-bus-channels*)
                 refs (id "Meters") (freq 5) (hop-size 2) (audio-bus 0))
  (sleep 1)
  (dotimes (idx num)
    (env-monometer freq (aref refs idx)
                   (+ audio-bus idx) hop-size
                   :action (lambda (n)
                             (push (node-id n)
                                   *node-ids*))
                   :tail group)))

#|

(meters :id "meters" :num 8)

(time (let ((meter (svref (cuda-gui::meters (cuda-gui::find-gui "meters")) 0)))
 (nrt-funcall
  (lambda ()
    (cuda-gui:change-level meter (random 100))))))

(trace cuda-gui::paint-event)

(let ((meter (svref (cuda-gui::meters (cuda-gui::find-gui "meters")) 0)))
 (nrt-funcall
  (lambda ()
    (cuda-gui:change-level meter (random 100)))))


(time (let ((meter (svref (cuda-gui::meters (cuda-gui::find-gui "meters03")) 0)))
 (cuda-gui:change-level meter (random 100))))

(time (let ((meter (svref (cuda-gui::meters (cuda-gui::find-gui "meters03")) 0)))
        (cuda-gui:change-level meter (random 100))))

(defparameter *test* (cuda-gui:meter-gui :num 2 :dsp-node-ids '() :id 10))

(incudine-gui::meters *test*)

(dsp! monometer (freq (gui incudine-gui::levelmeter) (chan channel-number) (gui-idx channel-number))
   (:defaults 10 nil 0 0)
   (levelmeter (audio-in chan) freq gui))

(defun meters (&key (num 2) (id "Meters") (freq 10) (audio-bus 0))
  (let* ((gui (cuda-gui:meter-gui :num num :dsp-node-ids '() :id id)))
    (loop
       for idx below num
       do (progn
            (monometer freq (svref (incudine-gui::meters gui) idx) (+ audio-bus idx) idx)
            :action (lambda (n)
                      (push (node-id n)
                            (cuda-gui:node-ids gui)))))))

(env-meters :num 8 :id "meters01" :freq 10 :hop-size 1)
(env-meters :num 8 :id "meters02" :freq 10 :hop-size 2)
(env-meters :num 16 :id "meters03" :freq 10 :hop-size 4)
(env-meters :num 16 :id "meters04" :freq 20 :hop-size 4)

|#

;;(setf (logger-level) :debug)

