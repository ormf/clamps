;;; 
;;; amp.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :of-incudine-dsps)

(define-vug limiter (in amp dur)
  (:defaults 0 1.0 0.01)
  (with ((bufsize (sample->fixnum (* dur *sample-rate*)))
         (buf (make-frame (* 3 bufsize) :zero-p t))
         (inpos 0) (midpos bufsize) (outpos (* 2 bufsize)) (tmppos 0)
         (flips 0)
         (prev-maxval +sample-zero+) (curr-maxval +sample-zero+)
         (maxval2 +sample-zero+)
         (level 1.0d0) (next-level 1.0d0)
         (slope 0.0d0)
         (slope-factor (float (/ bufsize) 1.0d0))
         (pos 0) (val +sample-zero+))
    (declare
     (type frame buf)
     (type sample
           slope-factor prev-maxval curr-maxval maxval2 level next-level
           slope slope-factor val)
     (type incudine::non-negative-fixnum
           bufsize inpos midpos outpos tmppos flips pos))
    (prog1
        (if (< flips 2)
            0.0d0
            (* level (frame-ref buf (+ outpos pos))))
      (setf (frame-ref buf (+ inpos pos)) in
            val (abs in))
      (when (> val curr-maxval) (setf curr-maxval val))
      (incf level slope)
      (incf pos)
      (when (>= pos bufsize)
        (setf pos 0
              maxval2 (max prev-maxval curr-maxval)
              prev-maxval curr-maxval curr-maxval +sample-zero+
              next-level (if (> maxval2 amp) (/ amp maxval2) 1.0d0)
              slope (* (- next-level level) slope-factor)
              tmppos outpos outpos midpos midpos inpos inpos tmppos)
        (when (< flips 2) (incf flips))))))

(define-vug peak-limiter (in amp dur (peak-ref-trigger-fn function))
  (:defaults 0 1.0 0.01 (lambda ()))
  (with ((bufsize (sample->fixnum (* dur *sample-rate*)))
         (double-bufsize (* 2 bufsize))
         (buf (make-frame (* 3 bufsize) :zero-p t))
         (inpos 0) (midpos bufsize) (outpos (* 2 bufsize)) (tmppos 0)
         (flips 0)
         (prev-maxval +sample-zero+) (curr-maxval +sample-zero+)
         (maxval2 +sample-zero+)
         (level 1.0d0) (next-level 1.0d0)
         (slope 0.0d0)
         (slope-factor (float (/ bufsize) 1.0d0))
         (pos 0) (val +sample-zero+))
    (declare
     (type frame buf)
     (type sample
           slope-factor prev-maxval curr-maxval maxval2 level next-level
           slope slope-factor val)
     (type incudine::non-negative-fixnum
           bufsize double-bufsize inpos midpos outpos tmppos flips pos))
    (prog1
        (if (< flips 2)
            0.0d0
            (* level (frame-ref buf (+ outpos pos))))
      (setf (frame-ref buf (+ inpos pos)) in
            val (abs in))
      (when (> val amp)
        (nrt-funcall (lambda () (at (+ (now) (* 2 bufsize)) peak-ref-trigger-fn))))
      (when (> val curr-maxval) (setf curr-maxval val))
      (incf level slope)
      (incf pos)
      (when (>= pos bufsize)
        (setf pos 0
              maxval2 (max prev-maxval curr-maxval)
              prev-maxval curr-maxval curr-maxval +sample-zero+
              next-level (if (> maxval2 amp) (/ amp maxval2) 1.0d0)
              slope (* (- next-level level) slope-factor)
              tmppos outpos outpos midpos midpos inpos inpos tmppos)
        (when (< flips 2) (incf flips))))))


#|
(dsp! pulse-osc ()
  (foreach-frame
    (let ((sig (reduce-warnings
                 (oscrs 500
                        (decay-2 (impulse  8 1 (* 0.6 (phasor 0.25)))
                                 0.001 0.3)))))
      (out sig (limiter sig 0.4)))))

(pulse-osc :tail 200)

(dsp! peak-pulse-osc ((peak-trigger-fn function))
  (foreach-frame
    (let ((sig (reduce-warnings
                 (oscrs 500
                        (decay-2 (impulse  8 (* 0.6 (phasor 0.25)))
                                 0.001 0.3)))))
      (out sig (peak-limiter sig 0.4 0.01 peak-trigger-fn))))
  )

(defparameter *my-bang* (make-bang (lambda () (clamps:imsg :warn "peaking"))))

(defun speedlim-trigger-fn (timeout bang)
  (let (pending)
    (lambda ()
      (unless pending
        (trigger bang)
        (set-val bang 1)
        (setf pending t)
        (at (+ (now) (* *sample-rate* timeout))
            (lambda ()
              (set-val bang 0)
              (setf pending nil)))))))

;;; (peak-pulse-osc (speedlim-trigger-fn 1 *my-bang*) :tail 200)
|#
