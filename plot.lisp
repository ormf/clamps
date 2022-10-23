;;; 
;;; plot.lisp
;;;
;;;
;;; simple interface to gnuplot. Depends on cm and uiop. Load with
;;; (ql:quickload "cm") (ql:quickload "uiop")
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

(in-package :cm)

(defparameter *gnuplot-program* "/usr/bin/gnuplot")
(defparameter *gnuplot-options* "notitle with lines;")
(defparameter *gnuplot-header* nil)

(defun construct-plot-command (&key region (grid t) (header *gnuplot-header*) (options *gnuplot-options*) &allow-other-keys)
  (concatenate 'string
               (if grid (format nil "set grid xtics lt 1 lc rgb \"#bbbbbb\";set grid ytics lt 1 lc rgb \"#bbbbbb\";~%") "")
               (if header (format nil "~a~%" header) "")
               "plot "
               (if region (format nil "[~{~,2f~^:~}] " region) "")
               "'<cat' "
               options))

(defgeneric plot (data &rest args &key region header options grid &allow-other-keys))

(defmethod plot ((data list) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as a list of data lists with > 2 elements of
type number. Interprets the first two elements of the sublists as x y
pairs.
  (declare (ignore header options grid))"
  (let* ((region (or region (let ((x-vals (mapcar #'first data)))
                              (list
                               (float (apply #'min x-vals) 1.0)
                               (float (apply #'max x-vals) 1.0)))))
         (gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (format out "~{~{~a ~}~%~}" data))))

(defmethod plot ((fn function) &rest args
                 &key (region '(0 1)) (header *gnuplot-header*) (options *gnuplot-options*) (num-values 100) (grid t))
  "Plot function (has to be a function accepting 1 argument). :region specifies xmin and xmax (default (0 1)),
:num-values the number of values to plot (default 100)."
  (declare (ignore header options grid))
  (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream))
         (data (destructuring-bind (xmin xmax) region
                 (loop
                    for count below (1+ num-values)
                    collect
                      (let ((x (+ xmin (/ (* count (- xmax xmin)) num-values))))
                        (list (float x 1.0) (float (funcall fn x) 1.0)))))))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (format out "~{~{~a ~}~%~}" data))))

(defmethod plot ((obj simple-array) &rest args &key region (header *gnuplot-header*) (options *gnuplot-options*) (grid t))
  "Plot input data given as a one-dimensional array. :region specifies array-bounds (rounded to nearest integer)."
  (declare (ignore header options grid))
  (let* ((gnuplot-instance
          (uiop:launch-program
           (list *gnuplot-program*  "-p" "-e"
                 (apply #'construct-plot-command :region region args))
           :input :stream)))
    (with-open-stream (out (uiop:process-info-input gnuplot-instance))
      (destructuring-bind (start end) (or region `(0 ,(1- (length obj))))
        (loop for idx from (round start) to (round end)
           do (format out "~a ~a~%" idx (float (aref obj idx) 1.0)))))))

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

(plot (incudine::make-envelope '(0.01 1 1 0.01) '(1 2 1) :curve '(:exp :lin :exp)))
(plot (incudine::make-envelope '(0 1 1 0) '(1 2 1)))

(defparameter *buffer* (incudine::make-buffer 100))

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

(incudine::dsp! play-envelope ((env incudine::envelope) timescale)
  (incudine::out (incudine.vug::envelope env :time-scale timescale)))

(let ((env-buffer (incudine::bounce-to-buffer (*buffer* :frames 100 :sample-rate 99)
                    (simple 1 1 0))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx)))



(let* ((envelope (incudine::make-envelope '(0 1 1 0) '(1 2 1)))
       (env-dur (incudine::envelope-duration envelope))
       (env-buffer (incudine::make-buffer 100)))
  (incudine::bounce-to-buffer (env-buffer :frames 100 :sample-rate 99)
    (incudine::out (incudine.vug::envelope envelope :time-scale (/ env-dur))))
  (loop for idx below 100 collect (incudine::buffer-value env-buffer idx)))



(export 'plot 'cm)

#|

Examples:

(plot #'sin :region `(0 ,(* 2 pi)))

(plot #'exp :region `(0 4))

;; default region is '(0 1)

(plot #'sin)

(plot
 (loop for count below 20
    collect (list count (random 40.0)))
 :grid nil)

|#
