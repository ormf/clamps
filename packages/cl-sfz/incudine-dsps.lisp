;;; 
;;; incudine.dsps.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

(define-vug buffer-loop-play ((buffer buffer) rate start-pos
                              loopstart loopend)
  (buffer-read buffer (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(defun abs-path (sample-path sfz-file-path)
  (merge-pathnames sample-path sfz-file-path))

(defun get-keynum (entry)
  (sample (- (getf entry :pitch-keycenter) (/ (or (getf entry :tune) 0) 100))))

(defstruct lsample
  "structure for a sample with two loop-points. The structure also
contains a slot for the sample buffer data."
  filename
  buffer
  (play-fn #'play-lsample)
  (keynum +sample-zero+ :type sample)
  (loopstart +sample-zero+ :type sample)
  (amp (sample 1.0) :type sample)
  (loopend +sample-zero+ :type sample))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample 440.0d0) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(defun collect-props (names plist)
  (loop for name in names
        collect `(,name (getf ,plist ,(alexandria:make-keyword name)))))

(defmacro with-props ((&rest names) plist &rest body)
  `(let ,(collect-props names plist)
     ,@body))

(defparameter *env1* (make-envelope '(0 1 1 0) '(0.1 .9 .1)))

#|
(define-ugen phasor* frame (freq init)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor freq init)))
    frm))
|#

(define-vug buffer-loop-play ((buffer buffer) rate start-pos
                              loopstart loopend)
  (buffer-read buffer (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(define-vug buffer-play ((buffer buffer) start-pos end-pos dur)
  (buffer-read buffer (line start-pos end-pos dur)
               :interpolation :cubic))

(dsp! lsample-play ((buffer buffer) dur amp rate pan loopstart loopend startpos)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 1 1 1 0.5 0 44100 0)
  (foreach-channel
    (cout
     (pan2
      (* amp 
	 (envelope *env1* 1 dur #'free)
	 (buffer-loop-play buffer rate (* startpos *sample-rate*) loopstart loopend))
      pan))))

(dsp! sample-play ((buffer buffer) dur amp rate pan startpos)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 1 1 1 0.5 0)
  (with-samples ((start (* startpos *sample-rate*))
                 (duration (/ dur rate))
                 (end (* *sample-rate* (+ startpos dur))))
    (foreach-channel
      (cout
       (pan2
        (* amp 
	   (envelope *env1* 1 duration #'free)
	   (buffer-play buffer start end duration))
        pan)))))

(export '(lsample sample-play lsample-play lsample-filename lsample-buffer
          lsample-play-fn lsample-keynum lsample-loopstart
          lsample-amp lsample-loopend)
        'of-incudine-dsps)

