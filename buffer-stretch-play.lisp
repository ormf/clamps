(in-package :of-incudine-dsps)

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample 440.0d0) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(define-vug stereo (in) (out in in))

(define-vug buffer-stretch-play ((buffer buffer) rate wwidth start end stretch)
  (with-samples ((myrate (/ rate))
                 (wsamps (* wwidth *sample-rate* 0.001d0))
                 (phfreq (/ 1000.0d0 myrate wwidth))
                 (ph1 (phasor phfreq 0))
                 (ph2 (phasor phfreq 0.5))
                 (mainpt (line (* start *sample-rate*)
                               (*  end *sample-rate*)
                               (* stretch (- end start))
                               #'free)))
    (progn
      mainpt
      (+
       (* (buffer-read *hanning1024* (* ph1 1024))
          (buffer-read buffer (+ (samphold mainpt ph1) (* ph1 wsamps))))
       (* (buffer-read *hanning1024* (* ph2 1024))
          (buffer-read buffer
                       (max 0.0d0
                            (+
                             (samphold mainpt ph2 (* -0.5 wsamps) -1)
                             (* ph2 wsamps)))))))))

;;; (play-sol-sample-preset-stretch 59 1 0 0 5 0.5 137)

#|
(dsp! play-buffer-stretch ((buffer buffer) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end)))
      (stereo (* ampl
                 (envelope *env1* 1 (* stretch (- ende start)) #'free)
                 (buffer-stretch-play buffer rate wwidth start ende stretch))))))
|#

(dsp! play-buffer-stretch ((buffer buffer) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope *env1* 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (stereo sig))))

(dsp! play-buffer-stretch-out ((buffer buffer) amp transp start end stretch wwidth (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope *env1* 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
          (incf (audio-out out) sig))))



;;; (defparameter *my-buf* (buffer-load "/home/orm/work/kompositionen/letzte-worte/snd/fl-s01-line01.wav"))

;; (play-buffer-stretch :buffer *my-buf* :amp 0 :transp 0 :start 0 :end 0 :stretch 1 :wwidth 137 :out 1)
;; (play-buffer-stretch :buffer *my-buf* :amp 0 :transp -12 :start 0 :end 0 :stretch 1 :wwidth 137)




(declaim (inline make-oasr))
(defun make-oasr (suswidth suspan sustain-level
                 &key (curve -4) base restart-level
                 (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with ATTACK-TIME, SUSTAIN-LEVEL
and RELEASE-TIME.

The curvature CURVE defaults to -4."
  (make-envelope (list 0 sustain-level 0) (list (* suspan (1- suswidth))
                                                (* (1- suspan) (1- suswidth)))
                 :curve curve :base base :release-node 1
                 :restart-level restart-level :real-time-p real-time-p))

(declaim (inline make-fasr))
(defun make-fasr (attack-time sustain-level release-time dur
                  &key (curve (list 1 0 -1)) base restart-level
                 (real-time-p (allow-rt-memory-p)))
  "Create and return a new ENVELOPE structure with ATTACK-TIME, SUSTAIN-LEVEL
and RELEASE-TIME.

The curvature CURVE defaults to -4."
  (make-envelope (list 0 sustain-level sustain-level 0) (list attack-time (- dur attack-time release-time) release-time)
                 :curve curve :base base :release-node -1
                 :restart-level restart-level :real-time-p real-time-p))

(dsp! play-buffer-stretch-env-out ((buffer buffer) amp transp start end stretch wwidth attack release (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0 0.01 0)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             (min (/ (buffer-frames buffer) *sample-rate*) end)))
                   (sig (* (envelope (reduce-warnings (make-fasr attack ampl release (* stretch (- ende start))))
                                     1 1 #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (incf (audio-out out) sig))))

;;; (format t "~a , ~a ~a ~a ~a ~a ~a~%" (* stretch (- ende start)) buffer rate wwidth start ende stretch)


(dsp! play-buffer-stretch-env-pan-out ((buffer buffer) amp transp start end stretch wwidth attack release pan (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0 0.01 0 0)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end)))
                 (sig (* (envelope (reduce-warnings (make-fasr attack ampl release (* stretch (- ende start))))
                                   1 1 #'free)
                         (buffer-stretch-play buffer rate wwidth start ende stretch))))
    (incf (audio-out out) (* sig left))
    (incf (audio-out (mod (+ 1 out) 8)) (* sig right))))

