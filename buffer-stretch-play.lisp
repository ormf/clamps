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
  (with-samples ((wsamps (* wwidth *sample-rate* 0.001d0))
                 (phfreq (/ 1000.0d0 rate wwidth))
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

(dsp! play-buffer-stretch ((buffer buffer) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137)
  (with-samples ((rate (reduce-warnings (/ 8.175798915643707d0
                                           (keynum->hz transp))))
                 (ampl (db->lin amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end)))
      (stereo (* ampl
                 (envelope *env1* 1 (* stretch (- ende start)) #'free)
                 (buffer-stretch-play buffer rate wwidth start ende stretch))))))

;;; (defparameter *my-buf* (buffer-load "/tmp/test.wav"))


;;;  (play-buffer-stretch *my-buf* 0 -12 2 0 20.5 2 137)

