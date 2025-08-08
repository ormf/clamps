;;; 
;;; vbap-dsps.lisp
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

(define-ugen vbap frame (in (vbap vbap) azi ele spread) "Incudine Ugen for vbap/mdap. Returns a foreign array of amplitude
values for the speakers of the /vbap/ struct with /azi/, /ele/ and
/spread/ applied.

@Examples
(dsp! pnoise-vbap-test ((vbap vbap) freq amp azi ele spread)
  (:defaults (incudine-missing-arg 'vbap) 10 0.1 0 0 0)
  (with ((numoutchannels (vbap-data-num-speakers (vbap-vbap-data vbap))))
    (foreach-frame
      (with ((frame (vbap (white-noise (* amp (decay-2 (impulse freq) 0 (/ 0.5 freq))))
                           vbap azi ele spread)))
        (maybe-expand frame)
        (dochannels (current-channel numoutchannels)
          (setf (audio-out current-channel)
                (frame-ref frame current-channel)))))))

(let* ((ls-directions '(-30 30 70 110 150 -150 -110 -70))
       (vbap (init-vbap ls-directions))
       (time -0.5))
  (pnoise-vbap-test vbap :azi -30 :id 10 :head 200)
  (dolist (azi ls-directions)
    (let ((azi azi))
      (at (+ (now) (clamps:time->samps (incf time 0.5)))
          (lambda () (set-control 10 :azi azi))))))

;;; DSP for an automated circular movement of noise pulses in 5 degree steps

(dsp! pnoise-circle-vbap-test ((vbap vbap) pulse-freq circle-freq amp spread)
  (:defaults (incudine-missing-arg 'vbap) 10 0.5 0.1 0)
  (with ((numoutchannels (vbap-data-num-speakers (vbap-vbap-data vbap)))
         (first-speaker-angle (reduce-warnings (/ (first (vbap-ls-directions vbap)) 5))))
    (foreach-frame
      (reduce-warnings
        (with ((azi (* 5.0d0 (round (+ (* 72 (phasor circle-freq 0)) first-speaker-angle))))
               (frame (vbap (white-noise (* amp (decay-2 (impulse pulse-freq) 0 (/ 0.5 pulse-freq))))
                            vbap azi 0 spread)))
          (maybe-expand azi)
          (maybe-expand frame)
          (dochannels (current-channel numoutchannels)
            (incf (audio-out current-channel)
                  (frame-ref frame current-channel))))))))

;;; Use MIDI Controllers with ccnum 0-3 to control the spread,
;;; circle-freq, pulse-freq and amplitude:

(defparameter *vbap-ctl-unwatch* nil)

(progn ;; init controllers
  (setf (ccin 0) 0)
  (setf (ccin 1) 64)
  (setf (ccin 2) 64)
  (setf (ccin 3) 64))

(pnoise-circle-vbap-test
    (init-vbap '(-30 30 70 110 150 -150 -110 -70))
    :action
    (lambda (node)
      (let ((id (node-id node)))
        (add-watch
         *vbap-ctl-unwatch*
         (set-control id :spread (m-lin (ccin 0) 0 100))
         (set-control id :circle-freq (m-exp (ccin 1) 0.1 1))
         (set-control id :pulse-freq (m-exp (ccin 2) 5 20))
         (set-control id :amp (m-exp-zero (ccin 3) 0.01 1)))))
    :free-hook (list (lambda (node) node (unwatch-all *vbap-ctl-unwatch*)))
    :head 200)

@See-also
vbap-bus
vbap-pair-bus
"
  (:defaults 0 (incudine-missing-arg 'vbap) 0 0 0)
  (with ((num-outputs (vbap-data-num-speakers (vbap-vbap-data vbap)))
         (outputs (make-frame num-outputs))
         (ls-gains (vbap-ls-gains vbap))
         (last-azi most-negative-short-float)
         (last-ele most-negative-short-float)
         (last-spread most-negative-short-float))
    (when (or (when (/= last-azi azi)
                (setf last-azi azi)
                (setf (vbap-azi vbap) azi))
              (when (/= last-ele ele)
                (setf last-ele ele)
                (setf (vbap-ele vbap) ele))
              (when (/= last-spread spread)
                (setf last-spread spread)
                (setf (vbap-spread vbap) spread)))
      (nrt-funcall (lambda () (calc-vbap vbap))))
    (dotimes (i num-outputs)
      (setf (frame-ref outputs i) (* in (sample (getter (aref ls-gains i))))))
    outputs))

(dsp! vbap-bus ((in-bus-idx channel-number) (vbap vbap) azi ele spread)
  "Incudine DSP function distributing a bus signal to the physical
outlets using vbap/mdap. Works with any blocksize.

@Arguments
in-bus-idx - Non-negative Integer denoting the Input Bus index.
vbap - initialized Vbap Struct.
azi - Number denoting azimuth in degrees.
ele - Number denoting elevation in degrees (3D only).
spread - Number denoting spread in degrees. Will be clipped to the range [0..100].

@See-also
calc-vbap
init-vbap
vbap-pair-bus
"
  (:DEFAULTS 0 (incudine-missing-arg 'vbap) 0 0 0)
  (with ((numoutchannels (vbap-data-num-speakers (vbap-vbap-data vbap))))
    (declare (type channel-number numoutchannels))
    (reduce-warnings
      (foreach-frame
        (with ((frame (vbap (input-bus in-bus-idx) vbap azi ele spread)))
          (maybe-expand frame)
          (dochannels (current-channel numoutchannels)
            (setf (audio-out current-channel)
                  (frame-ref frame current-channel))))
        (setf (input-bus in-bus-idx) +sample-zero+)))))

(dsp! vbap-pair-bus ((in-bus-idx channel-number) (vbap1 vbap) (vbap2 vbap) azi1 ele1 spread1 azi2 ele2 spread2 pan)
  "Incudine DSP function distributing a bus signal to the physical
outlets using vbap/mdap with a pair of two vbaps with distinct
ele/azi/spread values and a pan value to interpolate between
them. This enables panning between disjunct speakers/directions. The
two provided vbaps should use the same speaker-setup. Works with any
blocksize.

@Arguments
in-bus-idx - Non-negative Integer denoting the Input Bus index.
vbap1 - initialized Vbap Struct for first direction.
vbap2 - initialized Vbap Struct for second direction.
azi1 - Number denoting azimuth in degrees of first direction.
ele1 - Number denoting elevation in degrees of first direction (3D only).
spread1 - Number denoting spread in degrees of first direction. Will be clipped to the range [0..100].
azi2 - Number denoting azimuth in degrees of second direction.
ele2 - Number denoting elevation in degrees of second direction (3D only).
spread2 - Number denoting spread in degrees of second direction. Will be clipped to the range [0..100].
pan - Number denoting the interpolation between direction1 and direction2 wrapped into the range [0..1].

@See-also
calc-vbap
init-vbap
vbap-bus
"
  (:DEFAULTS 0 (incudine-missing-arg 'vbap) (incudine-missing-arg 'vbap) 0 0 0 0 0 0 0)
  (with ((numoutchannels (vbap-data-num-speakers (vbap-vbap-data vbap1)))
         (last-ele1 -1.0d0)
         (last-azi1 -1.0d0)
         (last-spread1 -1.0d0)
         (last-ele2 -1.0d0)
         (last-azi2 -1.0d0)
         (last-spread2 -1.0d0)
         (out-gains1 (vbap-ls-gains vbap1))
         (out-gains2 (vbap-ls-gains vbap2)))
    (declare (type sample last-ele1 last-azi1 last-spread1
                   last-ele2 last-azi2 last-spread2)
             (type channel-number numoutchannels))
    (with-samples ((left (tick (cos (* pan +half-pi+))))
                   (right (tick (sin (* pan +half-pi+)))))
      (reduce-warnings
        (when (or (/= (tick azi1) last-azi1)
                  (/= (tick ele1) last-ele1)
                  (/= (tick spread1) last-spread1)
                  (/= (tick azi2) last-azi2)
                  (/= (tick ele2) last-ele2)
                  (/= (tick spread2) last-spread2))
          (setf last-azi1 azi1)
          (setf last-ele1 ele1)
          (setf last-spread1 spread1)
          (setf last-azi2 azi2)
          (setf last-ele2 ele2)
          (setf last-spread2 spread2)
          (nrt-funcall
           (lambda ()
             (setf (vbap-azi vbap1) (float azi1 1.0))
             (setf (vbap-ele vbap1) (float ele1 1.0))
             (setf (vbap-spread vbap1) (float spread1 1.0))
             (setf (vbap-azi vbap2) (float azi2 1.0))
             (setf (vbap-ele vbap2) (float ele2 1.0))
             (setf (vbap-spread vbap2) (float spread2 1.0))
             (calc-vbap vbap1)
             (calc-vbap vbap2)))))
      (reduce-warnings
        (foreach-frame
          (dochannels (current-channel numoutchannels)
            (setf (audio-out current-channel)
                  (float
                   (* (input-bus in-bus-idx)
                      (* left (getter (aref out-gains1 current-channel)))
                      (* right (getter (aref out-gains2 current-channel))) 1.0d0))))
          (setf (input-bus in-bus-idx) +sample-zero+))))))
