;;;
;;; basic-dsps.lisp
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

(in-package #:of-incudine-dsps)

(deftype non-negative-fixnum () `(integer 0 ,most-positive-fixnum))

;;; experimental aux bus:
#|
(defvar *aux* (incudine.external:foreign-alloc-sample
               (* 256 *number-of-input-bus-channels*)))

(declaim (inline aux))
(defun aux (n)
  (smp-ref *aux* n))

(declaim (inline set-aux))
(defun set-aux (n value)
  (setf (smp-ref *aux* n) (sample value)))

(defsetf aux set-aux)
|#

(define-vug ilag (in time)
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME, intialized with the first received in value."
  (pole* in (delay1 (t60->pole time))))

(define-ugen envelope* frame ((env incudine.vug:envelope) gate time-scale (done-action function))
  "Envelope Ugen working with any blocksize. The product of /time-scale/
and the total duration of /env/ is the total duration of the envelope
in seconds. /done-action/ is called when the total-duration has been
reached or when /gate/ is zero and the release phase of the envelope
has ended.

envelope* returns an array of block-size samples.

@Arguments
env - incudine.vug:envelope instance to use.
gate - Number functioning as a gate: If zero, start the release phase.
time-scale - Number scaling the envelope x-values.
done-action - Function to call on the dsp-node at end of release.

@See-also
buffer-loop-play*
buffer-play*
buffer-stretch-play*
envelope*
line*
phasor*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
"
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (envelope env gate time-scale done-action)))
    frm))

(define-ugen line* frame (start end duration (done-action function))
  "Ugen of a line working with any block size.
@Arguments
start - Number denoting start value.
end - Number denoting end value-
duration - Number denoting duration in seconds.

@See-also
buffer-loop-play*
buffer-play*
buffer-stretch-play*
envelope*
phasor*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
"
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (line start end duration done-action)))
    frm))

(define-ugen phasor* frame (freq init)
  "Ugen of a phasor working with any block size.
@Arguments
freq - Number denoting frequency in Hz.
init - Number denoting initial phase.

@See-also
buffer-loop-play*
buffer-play*
buffer-stretch-play*
envelope*
line*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
"
  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor freq init)))
    frm))

(define-ugen phasor-amp* frame (freq init amp)
  "Incudine Ugen producing a normalized moving phase value with
frequency FREQ, initial value INIT (0 by default) and AMP at k-rate."
  (:defaults 1 0 1)
  (with ((frame (make-frame (block-size))))
    (with-samples ((rate (* amp freq *sample-duration*)))
      (foreach-frame
        (setf (frame-ref frame current-frame)
              (incudine.vug::%phasor rate init amp)))
      frame)))

(define-vug phasor-amp (freq init amp)
  "Incudine Ugen producing a normalized moving phase value with frequency
FREQ, initial value INIT (0 by default) and AMP."
  (:defaults 1 0 1)
  (with-samples ((rate (* amp freq *sample-duration*)))
    (incudine.vug::%phasor rate init amp)))

(dsp! osc~ (freq amp phase lagtime (buf buffer))
  "table lookup cosine oscillator."
  (:defaults 440 0.1 0 0.1 *COSINE-TABLE*)
  (foreach-frame
    (let ((sig (* (lag amp lagtime) (osc buf (lag freq lagtime) phase))))
      (out sig sig))))

(define-vug input-bus ((channel channel-number))
  (bus (the channel-number
         (+ (the channel-number
              (* current-frame *number-of-input-bus-channels*))
            channel))))

(define-vug bus-value ((channel fixnum))
  "if blocksize > 1 returns the value of bus for current-frame."
  (with ((num-frames (block-size)))
    (bus (the fixnum
              (+ (the fixnum
                      (* current-frame num-frames))
                 channel)))))

(dsp! cp-input-buses ((first-input channel-number) (first-bus channel-number)
                      (num-channels channel-number))
  "cp all audio inputs to buses starting at first-in-bus + bus-offset."
  (:defaults 0 0 *number-of-input-bus-channels*)
  (let ((numchans (min num-channels *number-of-input-bus-channels*)))
    (foreach-frame
      (dochannels (current-channel numchans)
        (setf (input-bus (+ current-channel first-bus))
              (audio-in (+ current-channel first-input)))))))

(dsp! cp-output-buses ((first-out-bus channel-number))
  "cp all audio outputs to buses starting at first-out-bus."
  (:defaults 8)
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (setf (input-bus (+ current-channel first-out-bus))
            (audio-out current-channel)))))

(dsp! bus-to-out ((numchannels channel-number) (startidx channel-number))
  (foreach-frame
    (dochannels (current-channel numchannels)
      (setf (audio-out current-channel)
            (input-bus (+ current-channel startidx))))))

(dsp! mix-bus-to-out ((startidx channel-number) (numchannels channel-number))
  (:defaults 16 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (incf (audio-out current-channel) (input-bus (+ current-channel startidx))))))

(dsp! clear-buses ((startidx channel-number) (numchannels channel-number))
  (:defaults 16 8)
  (foreach-frame
    (dochannels (current-channel numchannels)
      (setf (input-bus (+ current-channel startidx)) +sample-zero+))))

(define-vug counter ((start non-negative-fixnum) (end non-negative-fixnum) (done-action function))
  (:defaults 0 0 #'identity)
  (with ((i (clip start 0 end)))
    (declare (non-negative-fixnum i))
    (if (< i end)
        (prog1 i (incf i))
        (progn
          (funcall done-action (node))
          i))))

(dsp! buffer-record ((buf buffer) (env incudine.vug:envelope) (in channel-number)
                     (start fixnum) (frames fixnum))
  (:defaults
   (incudine:incudine-missing-arg "BUFFER")
   *env1*
   0 0 0)
  (with ((real-start (max 0 (min start (buffer-size buf))))
         (end (if (zerop frames)
                  (- (buffer-size buf) real-start)
                  (max 0 (min (+ start frames) (buffer-size buf)))))
         (frm1 (envelope* env 1 (/ (- end real-start) *sample-rate*) #'free)))
    (declare (fixnum real-start end))
    (maybe-expand frm1)
    (foreach-frame
      (buffer-write buf (counter start end :done-action #'free)
                    (* (frame-ref frm1 current-frame) (audio-in in))))))

#|
(defvar *aux* (incudine.external:foreign-alloc-sample
               (* 256 *number-of-input-bus-channels*)))

(declaim (inline aux))
(defun aux (n)
  (smp-ref *aux* n))

(declaim (inline set-aux))
(defun set-aux (n value)
  (setf (smp-ref *aux* n) (sample value)))

(defsetf aux set-aux)
|#


#|
(defmacro foreach-input-channel (&body body)
  (with-gensyms (i)
    `(dochannels (,i *number-of-input-bus-channels*)
       (let ((current-channel ,i))
         (declare (type channel-number current-channel)
                  (ignorable current-channel))
         ,@body))))

(declaim (inline bus))
(defun bus (num &optional (frame 0))
  "Return the value of the bus number NUM. Setfable."
  (declare (type bus-number num)
           (type non-negative-fixnum frame))
  (smp-ref incudine::*bus-pointer*
           (the non-negative-fixnum
                (+ (the non-negative-fixnum
                        (* frame *number-of-bus-channels*))
                   num))))

(declaim (inline set-bus))
(defun set-bus (num frame value)
  (declare (type bus-number num)
           (type non-negative-fixnum frame))
  (setf (smp-ref *bus-pointer*
                 (the non-negative-fixnum
                                (+ (the non-negative-fixnum
                                        (* frame *number-of-bus-channels*))
                                   num)))
        (sample value)))

(defsetf bus (num &optional (frame 0)) (value)
  `(set-bus ,num ,frame ,value))
|#

;;; (setup-io)
;;; (dump (node 0))
;;; (block-size)
;;; (set-rt-block-size 256)
;;; (rt-start)

#|
(define-vug input-bus ((channel fixnum))
  (bus (the fixnum
         (+ (the fixnum
              (* current-frame *number-of-input-bus-channels*))
            channel))))

(dsp! cp-input-buses ()
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (setf (input-bus current-channel)
            (audio-in current-channel)))))

(dsp! out-test ()
  (foreach-frame
    (dochannels (current-channel *number-of-input-bus-channels*)
      (cout (input-bus current-channel)))))
|#
