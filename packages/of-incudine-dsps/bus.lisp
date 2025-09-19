;;; 
;;; bus.lisp
;;;
;;; low level dsp infrastructure of audio buses (of size
;;; block-size). Audio Buses use incudine's buses, each Audio Bus
;;; occupying (block-size) consecutive incudine buses starting at
;;;
;;; (+ *number-of-ctl-bus-channels*
;;;    (* audio-bus-num (block-size)))
;;;
;;; A bus-amp-synth takes care of amp-ctl and a master-out synth
;;; routes the bus to a physical output and resets the bus to 0.
;;; anything contributing to the buses output has to be scheduled
;;; before the bus-amp-synth and any visualizers (vu-meter, scope)
;;; should be scheduled directly before ("pre-fader") or after the
;;; bus-amp-synth ("post-fader") and before the master-out synth. The
;;; buses should be added to a protected group (e.g. 300) whereas
;;; synths contributing to the bus should be scheduled before in an
;;; unprotected group (e.g. 200).
;;;
;;; Here is an example node diagram using a sine wave generator
;;; outputting to the bus:
;;;
;;; group 200
;;;     node 40
;;;       simple-bus 440.0d0 1.0d0 0 2
;;; group 300
;;;     node 20
;;;       bus-amp-synth 0 0.1d0 2
;;;     node 21
;;;       env-monometer 10 #<ref -43> 0 2
;;;     node 22
;;;       env-monometer 10 #<ref -43> 1 2
;;;     node 23
;;;       master-out 0 2 0
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(export '(master-out bus-amp bus-amp-dsp master-out-dsp) 'incudine)

;;; audio buses start after the control buses. Reserve the maximum
;;; number of control buses here. If the number needs to be changed,
;;; first free all nodes using audio buses, then change the number and
;;; restart all dsps using audio buses.

(defvar *number-of-ctl-bus-channels* 1024)

#|
(defmacro bus-pointer ()
  `*bus-pointer*)
|#

(declaim (inline audio-bus))
(defun audio-bus (channel &optional (frame 0))
  "Return the value of the CHANNEL of the bus buffer FRAME. Setfable.

CHANNEL and FRAME are zero-based.

If AUDIO-BUS is used within a VUG definition, FRAME is ignored.

No bounds checking."
  (declare (type channel-number channel)
           (type non-negative-fixnum frame))
  (with ((frames (block-size)))
    (declare (type non-negative-fixnum frames))
    (smp-ref *bus-pointer*
             (the non-negative-fixnum
                  (+ (the non-negative-fixnum
                          (* channel frames))
                     *number-of-ctl-bus-channels*
                     frame)))))

(declaim (inline set-audio-bus))
(defun set-audio-bus (channel frame value)
  "set FRAME of audio bus CHANNEL to VALUE."
  (declare (type channel-number channel)
           (type non-negative-fixnum frame)
           (type real value))
  (with ((frames (block-size)))
    (declare (type non-negative-fixnum frames))
    (setf (smp-ref *bus-pointer*
                   (the non-negative-fixnum
                        (+ (the non-negative-fixnum
                                (* channel frames))
                           *number-of-ctl-bus-channels*
                           frame)))
          (sample value))))

(defsetf audio-bus (channel &optional (frame 0)) (value)
  `(set-audio-bus ,channel ,frame ,value))

(dsp! bus-amp-synth ((bus channel-number) amp (num-channels channel-number))
  (:DEFAULTS 0 1 0)
  "Control AMP of NUM-CHANNELS consecutive buses starting at BUS."
  (reduce-warnings
    (let ((lag-amp (lag amp 0.05)))
      (foreach-frame
        (dochannels (current-channel num-channels)
          (setf (audio-bus (+ current-channel bus) current-frame)
                (* lag-amp (audio-bus (+ current-channel bus) current-frame))))))))

(dsp! master-out ((bus channel-number) (num-channels channel-number)
                  (channel-offset channel-number))
  (:DEFAULTS 0 1 0)
  "multichannel master bus of NUM-CHANNELS consecutive
bus numbers starting at BUS, routed to hardware audio outputs starting
at CHANNEL-OFFSET."
  (reduce-warnings
    (foreach-frame
      (dochannels (current-channel num-channels)
        (incf (audio-out current-channel) (audio-bus (+ current-channel bus) current-frame))
        (setf (audio-bus (+ current-channel bus) current-frame) +sample-zero+))))) ;;; reset audio-bus before next audiograph run.
    
(defun bus-amp-dsp (&key (group 300) id-callback (num-channels 1) (amp 1) (audio-bus 0))
  "wrapper around bs-amp-synth with a callback to register the node id after instantiation."
  (bus-amp-synth audio-bus amp num-channels
                 :action (lambda (n)
                           (funcall id-callback (node-id n)))
                 :tail group))

(defun master-out-dsp (&key (group 300) id-callback (num-channels 1) (channel-offset 0) (audio-bus 0))
  "wrapper around master-out with a callback to register the node id after instantiation."
  (master-out audio-bus num-channels channel-offset
              :action (lambda (n)
                        (funcall id-callback (node-id n)))
              :tail group))

(dsp! master-limit-out ((bus channel-number) (num-channels channel-number)
                        (channel-offset channel-number))
  (:DEFAULTS 0 1 0)
  "multichannel master bus of NUM-CHANNELS consecutive
bus numbers starting at BUS, routed to hardware audio outputs starting
at CHANNEL-OFFSET."
  (reduce-warnings
    (foreach-frame
      (dochannels (current-channel num-channels)
        (incf (audio-out current-channel) (audio-bus (+ current-channel bus) current-frame))
        (setf (audio-bus (+ current-channel bus) current-frame) +sample-zero+)))))


#|

example:

(dsp! stereo-master ((bus channel-number))
  (:defaults 0)
  (foreach-frame
    ;; Stereo output from the first blocksize*2 buses.
    (incf (audio-out 0) (audio-bus bus current-frame))
    (incf (audio-out 1) (audio-bus (1+ bus) current-frame))
    (setf (audio-bus bus current-frame) 0)
    (setf (audio-bus (1+ bus) current-frame) 0)))

(dsp! master ((bus channel-number) amp)
  (:DEFAULTS 0 1)
  (foreach-frame
    ;; Stereo output from the first blocksize*2 buses.
    (out (* amp (audio-bus bus current-frame))
         (* amp (audio-bus (1+ bus) current-frame)))
    (setf (audio-bus bus current-frame) 0)
    (setf (audio-bus (1+ bus) current-frame) 0)))

;;; block-size safe sine oscillator outputting to stereo bus

(dsp! simple-bus (freq amp (bus channel-number))
  (:defaults 440 0.1 0)
  (with ((ph (phasor* freq 0))
         (frames (block-size)))
    (declare (channel-number frames))
    (maybe-expand ph)
     (foreach-frame
      (with-samples ((sig (* amp 1 (sin (* +twopi+ (frame-ref ph current-frame))))))
        (incf (audio-bus bus current-frame) sig)
        (incf (audio-bus (1+ bus) current-frame) sig)))))

(master 0 1 :tail 200)
(simple-bus 440 1 0 :tail 200)
(simple-bus 440 1 2 :tail 200)
(simple-bus 660 1 4 :tail 200)
(simple-bus 230 1 2 :tail 200)

(dump (node 0))

(set-control 41 :amp 1)

|#
