;;; 
;;; lsample.lisp
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

(in-package :incudine)

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))
(defparameter *hanning1024* (make-buffer 1024 :fill-function (gen:hanning)))

(defstruct lsample
  "structure for a sample with two loop-points. The structure also
contains a slot for the sample buffer data."
  filename
  buffer
  (play-fn #'play-lsample)
  (keynum +sample-zero+ :type sample)
  (loopstart +sample-zero+ :type sample)
  (amp (sample 0) :type sample)
  (loopend +sample-zero+ :type sample)
  (id nil :type (or NULL fixnum)))

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample 440.0d0) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))



(defun play-lsample (lsample pitch db dur &key (pan 0.5) (startpos 0))
  "play lsample with given pitch, amp and duration with loop."
  (with-slots (buffer amp keynum loopstart loopend) lsample
    (let ((rate (incudine::sample (ou:ct->fv (- pitch keynum)))))
      (lsample-play buffer dur (ou:db->amp (+ amp db)) rate pan loopstart loopend startpos))))

(defun play-sample (lsample pitch db dur &key (pan 0.5) (startpos 0))
  "play lsample once with given pitch, amp and duration."
  (with-slots (buffer amp keynum) lsample
    (let ((rate (incudine::sample (ou:ct->fv (- pitch keynum)))))
      (sample-play buffer dur (ou:db->amp (+ amp db)) rate pan startpos))))


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


(define-vug buffer-play* ((buffer buffer) start-pos end-pos dur)
  (buffer-read buffer (line start-pos end-pos dur)
               :interpolation :cubic))

(define-ugen envelope* frame ((env envelope) gate time-scale (done-action function))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (envelope env gate time-scale done-action)))
    frm))

(define-ugen line* frame (start end duration (done-action function))
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (line start end duration done-action)))
    frm))

(define-ugen phasor* frame (freq init)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor freq init)))
    frm))

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

(define-ugen phasor-loop* frame (rate start-pos loopstart loopend)
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor-loop rate start-pos loopstart loopend)))
    frm))

(define-ugen buffer-loop-play* frame ((buffer buffer) rate start-pos
                                      loopstart loopend)
  (with ((frm (make-frame (block-size)))
         (ph1 (phasor-loop* rate start-pos loopstart loopend)))
    (maybe-expand ph1)
    (foreach-frame
      (let ((p1 (frame-ref ph1 current-frame)))
        (setf (frame-ref frm current-frame) (buffer-read buffer p1 :interpolation :cubic))))
    frm))

(dsp! play-lsample* ((buffer buffer) dur amp rate pan loopstart loopend startpos (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 1 0 1 0 0 0 0 0 1)
  (with-samples ((rate (* (/ (buffer-sample-rate buffer) *sample-rate*) rate))
                 (start (* startpos (buffer-sample-rate buffer)))
                 (ampl (db->linear amp))
                 (loopend (if (zerop loopend) (incudine::sample (buffer-frames buffer)) (incudine::sample loopend)))
                 (ende (/ (buffer-frames buffer) *sample-rate*))
                 (alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha)))
    (with ((frm1 (envelope* *env1* 1 dur #'free))
           (frm2 (buffer-loop-play* buffer rate start loopstart loopend)))
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (let ((sig (* ampl
                     (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))
            (incf (audio-out out2) (* sig right))
            (incf (audio-out out1) (* sig left)))))))

(define-ugen buffer-play2* frame ((buffer buffer) start end dur)
  (with ((frm (make-frame (block-size)))
         (ph1 (line* start end dur #'free)))
    (maybe-expand ph1)
    (foreach-frame
      (let ((p1 (frame-ref ph1 current-frame)))
        (setf (frame-ref frm current-frame) (buffer-read buffer p1 :interpolation :cubic))))
    frm))

(dsp! play-sample* ((buffer buffer) dur amp rate pan startpos (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 1 1 1 0.5 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (ampl (db->linear amp))
                 (bsr (incudine::sample (buffer-sample-rate buffer)))
                 (start (* startpos (buffer-sample-rate buffer)))
                 (rate (/ (* rate (buffer-sample-rate buffer)) *sample-rate*))
                 (end (min (- (buffer-frames buffer) 1.0d0)
                           (* (+ start (* *sample-rate* dur)) rate)))
                 (duration (/ (- end start) (* rate *sample-rate*))))
  (with ((frm1 (envelope* *env1* 1 dur #'free))
         (frm2 (buffer-play2* buffer start end duration))
         ;;           (frm2 (buffer-stretch-play* buffer rate 137 0 ende 1))
         )
    (maybe-expand frm1)
    (maybe-expand frm2)
    (foreach-frame
      (let ((sig (* ampl
                    (frame-ref frm1 current-frame)
                    (frame-ref frm2 current-frame))))
        (incf (audio-out out2) (* sig right))
        (incf (audio-out out1) (* sig left)))))))


(define-ugen buffer-stretch-play* frame
    ((buffer buffer) rate wwidth start end stretch)  
    (with-samples ((myrate (/ rate))
                   (wsamps (* wwidth *sample-rate* 0.001d0))
                   (phfreq (/ 1000.0d0 myrate wwidth)))    
      (with ((frm (make-frame (block-size)))
             (ph1 (phasor* phfreq 0))
             (ph2 (phasor* phfreq 0.5))
             (mainpt (line* (* start *sample-rate*)
                            (*  end *sample-rate*)
                            (* stretch (- end start))
                            #'free)))
        (maybe-expand ph1)
        (maybe-expand ph2)
        (maybe-expand mainpt)
        (foreach-frame
          (let ((p1 (frame-ref ph1 current-frame))
                (p2 (frame-ref ph2 current-frame))
                (mpt (frame-ref mainpt current-frame)))
            (setf (frame-ref frm current-frame)
                  (+
                   (* (buffer-read *hanning1024* (* p1 1024))
                      (buffer-read buffer (+ (samphold mpt p1) (* p1 wsamps))))
                   (* (buffer-read *hanning1024* (* p2 1024))
                      (buffer-read buffer
                                   (max 0.0d0
                                        (+
                                         (samphold mpt p2 (* -0.5 wsamps) -1)
                                         (* p2 wsamps)))))))))
        frm)))

(dsp! play-buffer-stretch-env-pan-out*
    ((buffer buffer) amp transp start end stretch wwidth attack release pan (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0 0.01 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                                           (* *sample-rate* 8.175798915643707d0))))
                 (ampl (db->linear amp))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end))))
    (with ((frm1 (envelope* *env1* 1 (* stretch (- ende start)) #'free))
           (frm2 (buffer-stretch-play* buffer rate wwidth start ende stretch)))
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (let ((sig (* ampl
                     (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))
            (incf (audio-out out2) (* sig right))
            (incf (audio-out out1) (* sig left)))))))



(export '(lsample play-lsample* play-sample* lsample-filename lsample-buffer
          lsample-play-fn lsample-keynum lsample-loopstart
          lsample-amp lsample-loopend play-lsample play-sample)
        'incudine)
