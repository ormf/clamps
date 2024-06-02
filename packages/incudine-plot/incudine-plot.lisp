;;; 
;;; incudine-plot.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :incudine-plot)

(defmethod plot ((obj envelope) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t) (num-values 1000)) "Plot input data given as an incudine envelope."
  (declare (ignore region header options grid))
    (with-gnuplot-instance (out . args)
      (let* ((env-dur (envelope-duration obj))
             (env-buffer (make-buffer num-values)))
        (incudine::bounce-to-buffer (env-buffer :frames num-values :sample-rate num-values)
          (play-envelope obj (/ env-dur)))
        (loop for idx below num-values
           do (format out "~a ~a~%"
                      (/ (* env-dur idx) (1- num-values))
                      (float (buffer-value env-buffer idx) 1.0)))))
    (values obj))
#|
   Example:

   (plot (make-envelope '(0 1 1 0) '(1 0.5 1) :curve '(:cubic :lin :cubic)))

|#

(defmethod plot ((buffer buffer) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t) (x-axis :samples) (num-values 1000))
  "Plot input data given as an incudine buffer. :x-axis can be
:samples (default) or :seconds"
  (declare (ignore header options grid))
  (let* ((region
          (if region
              (list (max 0 (first region))
                    (min (second region) (1- (buffer-frames buffer))))
              `(0 ,(buffer-frames buffer))))
         (buffer-samplerate (slot-value buffer 'incudine::sample-rate)))
    (setf (getf args :region)
          (case x-axis
            (:samples region)
            (:seconds (mapcar (lambda (x) (/ x incudine::*sample-rate*)) region))))
    (with-gnuplot-instance (out . args)
      (let* ((start (first region))
             (dur (- (second region) start)))
        (loop for plot-idx below num-values
           do (let ((idx (round (+ start (* (/ plot-idx (1- num-values)) dur)))))
                (case x-axis
                  (:samples
                   (format out "~,4f ~,4f~%"
                           idx
                           (float (buffer-value buffer idx) 1.0)))
                  (:seconds
                   (format out "~,4f ~,4f~%"
                           (/ idx buffer-samplerate)
                           (float (buffer-value buffer idx) 1.0)))))))))
  (values buffer))

(defmethod plot ((obj incudine::envelope) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as an incudine envelope."
  (declare (ignore header options grid))
    (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (let* ((env-dur (incudine::envelope-duration obj))
             (env-buffer (incudine::make-buffer 100)))
        (incudine::bounce-to-buffer (env-buffer :frames 100 :sample-rate 100)
          (play-envelope obj (/ env-dur)))
        (loop for idx below 100
           do (format out "~a ~a~%"
                      (/ (* env-dur idx) 100)
                      (float (incudine::buffer-value env-buffer idx) 1.0)))))))

;;; (plot (incudine::make-envelope '(0.01 1 1 0.01) '(1 2 1) :curve '(:exp :lin :exp)))
;;; (plot (incudine::make-envelope '(0 1 1 0) '(1 2 1)))

(defparameter *buffer* (incudine::make-buffer 100))

(incudine::dsp! play-envelope ((env incudine::envelope) timescale)
  (incudine::out (incudine.vug::envelope env :time-scale timescale)))



#|
   Example:

   (defparameter *my-buffer* (buffer-load "/tmp/test.wav"))

   (plot *my-buffer* :x-axis :seconds)


(plot (let ((env-buffer (incudine::bounce-to-buffer (*buffer* :frames 100 :sample-rate 99)
                    (simple 1 1 0))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx))))



(let* ((envelope (incudine::make-envelope '(0 1 1 0) '(1 2 1)))
       (env-dur (incudine::envelope-duration envelope))
       (env-buffer (incudine::make-buffer 100)))
  (incudine::bounce-to-buffer (env-buffer :frames 100 :sample-rate 99)
    (incudine::out (incudine.vug::envelope envelope :time-scale (/ env-dur))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx)))

(let ((env-buffer (incudine::bounce-to-buffer (*buffer* :frames 100)
                    (incudine.vug::envelope (incudine::make-envelope '(0 1 1 0) '(0 1 2 3)) :time-scale 1))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx)))

(let ((env-buffer (incudine::bounce-to-buffer (*buffer* :frames 100)
                    (incudine::make-envelope '(0 1 1 0) '(1 2 1)))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx)))


(incudine::envelope-duration (incudine::make-envelope '(0 1 1 0) '(0 1 2 3)))

                    (incudine.vug::envelope  :time-scale 1)

(incudine::dsp! simple (freq amp phase)
  (incudine::out (incudine::sine freq amp phase)))


|#
