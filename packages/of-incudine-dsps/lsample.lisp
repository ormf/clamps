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

(in-package :of-incudine-dsps)

(defvar *standard-pitch* 440.0
  "Reference tuning frequency for middle A, setfable.")

(defparameter *keynum-offset* 0
  "Keynum offset related to the ratio of <<standard-pitch>> in relation to
440 Hz. Gets readjusted by setf of standard-pitch in clamps.

@See-also
standard-pitch")

(defstruct lsample
  "Structure for a sample with two loop-points.

@Note
Normally the user shouldn't be dealing with a lsample struct
directly. It is used by the /sfz/ and /poolevent/ classes and
documented here for completeness and insight.

A lsample contains the following slots, accessible using the functions
/lsample-<slot-name>/:

=name= -- Filename of the sample source.

=buffer= -- Buffer of the sample data.

=oneshot= -- Boolean indicating whether not to loop the sample on playback.

=keynum= -- Double Float denoting original keynum of the recorded sample.

=loopstart= -- Double Float denoting the loop start for loop playback, defaulting to /+​sample-zero​+/.

=loopend= -- Double Float denoting the loop start for loop playback, defaulting to /+​sample-zero​+/.

=amp= -- Amplitude of recorded sample in dB, defaulting to /+​sample-zero​+/.

@See-also
sfz
"
  name
  buffer
  (oneshot nil :type boolean)
  (keynum +sample-zero+ :type sample)
  (loopstart +sample-zero+ :type sample)
  (amp (sample 0) :type sample)
  (loopend +sample-zero+ :type sample))

(defun lsample-pathname (lsample)
  "Return the full pathname of /lsample/.

@Arguments
lsample - Instance of type lsample.

@See-also
lsample
"
  (pathname
   (incudine:buffer-file (lsample-buffer lsample))))

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample *standard-pitch*) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))



(defun play-lsample (lsample pitch ampdb duration &key (pan 0.5) (startpos 0) (oneshot nil osp) out1 out2)
  "Play lsample with given /pitch/, /ampdb/ and /duration/, /pan/ and
/startpos/ with or without looping.

@Arguments
lsample - Instance of type lsample
pitch - Positive Number denoting pitch in Midicents.
ampdb - Positive number denoting amp in dB, mapping range [-100..0] to linear amp [0..1].
duration - Positive number denoting duration in seconds.
:pan - Number denoting equal power panorama in the range [0..1].
:startpos - Positive number denoting start position into the sample in seconds.
:oneshot - Boolean indicating whether not to loop the sample on playback.

@See-also
lsample
"
  (with-slots (buffer amp keynum loopstart loopend) lsample
    (let ((rate (if pitch (incudine::sample (ou:ct->fr (+ *keynum-offset* (- pitch keynum)))) 1))
          (oneshot (if osp oneshot (lsample-oneshot lsample)))
          (out1 (or out1 0))
          (out2 (or out2 (1+ out1))))
      (if oneshot
        (play-buffer* buffer oid:*env1* duration (+ amp ampdb) rate pan startpos out1 out2 :tail 200)
        (play-buffer-loop* buffer oid:*env1* duration (+ amp ampdb) rate pan loopstart loopend startpos out1 out2 :tail 200)))))

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

(define-vug buffer-play ((buffer buffer) start-pos end-pos dur)
  (buffer-read buffer (line start-pos end-pos dur)
               :interpolation :cubic))

(define-ugen envelope* frame ((env incudine.vug:envelope) gate time-scale (done-action function))
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
  (with ((rate (* rate (/ (buffer-sample-rate buffer) *sample-rate*)))
         (frm (make-frame (block-size)))
         (ph1 (phasor-loop* rate start-pos loopstart loopend)))
    (maybe-expand ph1)
    (foreach-frame
      (let ((p1 (frame-ref ph1 current-frame)))
        (setf (frame-ref frm current-frame) (buffer-read buffer p1 :interpolation :cubic))))
    frm))

(dsp! play-buffer-loop* ((buffer buffer) (env incudine.vug:envelope) dur amp rate pan loopstart loopend start (out1 fixnum) (out2 fixnum))
  "Play /buffer/ with /env/ for /dur/ seconds and /amp/ in dB at /rate/,
with /pan/ from /start/ seconds into the sample. Loop the playback
between /loopstart/ and /loopend/. /loopend/ of 0 denotes end of
buffer. Output will be panned between /out1/ and /out2/. All other
keywords of incudine dsps also apply. Works with any block size.

@Arguments
buffer - Incudine Buffer.
env - Incudine Envelope.
dur - Positive Number denoting duration.
amp - Positive Number denoting amplitude in dB.
rate - Positive Number denoting playback rate, adjusted for the sample rate of buffer.
pan - Number in the range [0..1] denoting panorama between out1 and out2.
loopstart - Positive Number denoting start of loop.
loopstart - Positive Number denoting end of loop.
out1 - Non Negative Integer denoting first output channel.
out2 - Non Negative Integer denoting second output channel.

@See-also
lsample
play-buffer*
play-lsample
"
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 0 0 1)
  (with-samples ((startframe (* start (buffer-sample-rate buffer)))
                 (ampl (db->linear amp))
                 (loopend (if (zerop loopend) (incudine::sample (buffer-frames buffer)) (incudine::sample loopend)))
                 (alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha)))
    (with ((frm1 (envelope* env 1 dur #'free))
           (frm2 (buffer-loop-play* buffer rate startframe loopstart loopend)))
      (maybe-expand frm1)
      (maybe-expand frm2)
      (foreach-frame
        (let ((sig (* ampl
                      (frame-ref frm1 current-frame)
                      (frame-ref frm2 current-frame))))
          (incf (audio-out out1) (* sig left))
          (incf (audio-out out2) (* sig right)))))))

(define-ugen buffer-play* frame ((buffer buffer) rate startframe endframe)
  (with ((rate (* rate (/ (buffer-sample-rate buffer) *sample-rate*)))
         (frm (make-frame (block-size)))
         (dur (/ (- endframe startframe) (* rate *sample-rate*)))
         (ph1 (line* startframe endframe dur #'free)))
    (maybe-expand ph1)
    (foreach-frame
      (let ((p1 (frame-ref ph1 current-frame)))
        (setf (frame-ref frm current-frame)
              (buffer-read buffer p1 :interpolation :cubic))))
    frm))

(dsp! play-buffer* ((buffer buffer) (env incudine.vug:envelope)
                    dur amp rate pan startpos (out1 fixnum) (out2 fixnum))
  "Play /buffer/ with /env/ for /dur/ seconds and /amp/ in dB at /rate/,
with /pan/ from /startpos/ seconds into the sample. Output will be panned
between /out1/ and /out2/. All other keywords of incudine dsps also
apply. Works with any block size.

@Arguments
buffer - Incudine Buffer.
env - Incudine Envelope.
dur - Positive Number denoting duration.
amp - Positive Number denoting amplitude in dB.
rate - Positive Number denoting playback rate, adjusted for the sample rate of buffer.
pan - Number in the range [0..1] denoting panorama between out1 and out2.
startpos - Positive Number denoting in seconds into the buffer.
out1 - Non Negative Integer denoting first output channel.
out2 - Non Negative Integer denoting second output channel.

@See-also
lsample
play-buffer-loop*
play-buffer-stretch-env-pan-out*
play-lsample
"
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 1)
  (with-samples (;;; (rate (* (/ (buffer-sample-rate buffer) *sample-rate*) rate))
                 (startframe (* startpos (buffer-sample-rate buffer)))
                 (ampl (db->linear amp))                 
                 (endpos (min (+ startpos dur) (/ (buffer-frames buffer) (buffer-sample-rate buffer))))
                 (endframe (* endpos (buffer-sample-rate buffer)))
                 (alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha)))
    (with ((frm1 (envelope* env 1 (- endpos startpos) #'free))
           (frm2 (buffer-play* buffer rate startframe endframe)))
      (maybe-expand frm1)
      (maybe-expand frm2)
      (foreach-frame
        (let ((sig (* ampl
                      (frame-ref frm1 current-frame)
                      (frame-ref frm2 current-frame))))
          (incf (audio-out out1) (* sig left))
          (incf (audio-out out2) (* sig right)))))))

#|
(dsp! play-sample* ((buffer buffer) (env incudine.vug:envelope) dur amp rate pan start (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (ampl (db->linear amp))
                 (rate (* rate (/ (buffer-sample-rate buffer) *sample-rate*)))
                 (startframe (* start (buffer-frames buffer)))
                 (endframe
                  (min (+ startframe (* dur rate *sample-rate*))
                       (buffer-frames buffer)))
                 (duration (* rate *sample-rate* (- endframe startframe))))
  (with ((frm1 (envelope* env 1 duration #'free))
         (frm2 (buffer-play* buffer rate startframe endframe)))
    (maybe-expand frm1)
    (maybe-expand frm2)
    (foreach-frame
      (let ((sig (* ampl
                    (frame-ref frm1 current-frame)
                    (frame-ref frm2 current-frame))))
        (incf (audio-out out2) (* sig right))
        (incf (audio-out out1) (* sig left)))))))
|#

(define-ugen buffer-stretch-play* frame
    ((buffer buffer) rate wwidth start end stretch)  
  (with-samples ((rate (* rate (/ (buffer-sample-rate buffer) *sample-rate*)))
                 (invrate (/ rate))
                 (wsamps (* wwidth *sample-rate* 0.001d0))
                 (phfreq (/ 1000.0d0 invrate wwidth)))    
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
    ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth attack release pan (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137 0 0.01 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                                           (* *sample-rate* 8.175798915643707d0))))
                 (ampl (db->linear amp))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end))))
    (with ((frm1 (envelope* env 1 (* stretch (- ende start)) #'free))
           (frm2 (buffer-stretch-play* buffer rate wwidth start ende stretch)))
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (let ((sig (* ampl
                     (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))
            (incf (audio-out out2) (* sig right))
            (incf (audio-out out1) (* sig left)))))))

