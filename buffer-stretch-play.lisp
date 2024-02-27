(in-package :of-incudine-dsps)

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert VALUE dB to linear value."
  (* (sample 440.0d0) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(define-vug stereo (in) (out in in))

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


#|

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

|#

(dsp! play-buffer-stretch* ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end)))
      (with (
             (frm1 (envelope* env 1 (* stretch (- ende start)) #'free))
             (frm2 (buffer-stretch-play* buffer rate wwidth start ende stretch))
             )
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (stereo (* ampl (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))))))

#|

(defparameter *buf* (incudine:buffer-load
 "~/work/kompositionen/big-orchestra/snd/bo-samples/aitken-mallet/einzeln-gsp/T03_1-239.wav"))

(rt-start)
(at (+ (now) (* *sample-rate* 1)) #'play-buffer-stretch* :buffer *buf* :end 0.2)
(play-buffer-stretch* :buffer *buf* :end 0.2)
(incudine::node-free-all)

|#

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
    (+
     (* (buffer-read *hanning1024* (* ph1 1024))
        (buffer-read buffer (+ (samphold mainpt ph1) (* ph1 wsamps))))
     (* (buffer-read *hanning1024* (* ph2 1024))
        (buffer-read buffer
                     (max 0.0d0
                          (+
                           (samphold mainpt ph2 (* -0.5 wsamps) -1)
                           (* ph2 wsamps))))))))

;;; (play-sol-sample-preset-stretch 59 1 0 0 5 0.5 137)

#|
(dsp! play-buffer-stretch ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth)
(:defaults (incudine:incudine-missing-arg "BUFFER")
           (incudine:incudine-missing-arg "ENV")
           0 0 0 0 1 137 0)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end)))
      (stereo (* ampl
                 (envelope env 1 (* stretch (- ende start)) #'free)
                 (buffer-stretch-play buffer rate wwidth start ende stretch))))))
|#



(dsp! play-buffer-stretch ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137)
  (with-samples ((rate (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                          (* *sample-rate* 8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope env 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (stereo sig))))


(dsp! play-buffer-stretch-out ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137 0)
  (with-samples ((rate (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                          (* *sample-rate* 8.175798915643707d0)))
                 (ampl (db->linear amp)))
    (with-samples ((ende (if (or (zerop end)
                                 (> end (/ (buffer-frames buffer) *sample-rate*)))
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope env 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
          (incf (audio-out out) sig))))



;;; (defparameter *my-buf* (buffer-load "/home/orm/work/kompositionen/letzte-worte/snd/fl-s01-line01.wav"))

;; (play-buffer-stretch-out :buffer *my-buf* :amp 0 :transp 0 :start 0 :end 0 :stretch 1 :wwidth 137 :out 1)
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

(dsp! play-buffer-stretch-env-pan-out ((buffer buffer) amp transp start end stretch wwidth attack release pan (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             0 0 0 0 1 137 0 0.01 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings
                         (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                            (* *sample-rate* 8.175798915643707d0))))
                 (ampl (db->linear amp))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end)))
                 (sig (* (envelope (reduce-warnings (make-fasr attack ampl release (* stretch (- ende start))))
                                   1 1 #'free)
                         (buffer-stretch-play buffer rate wwidth start ende stretch))))
    (foreach-frame
      (incf (audio-out out2) (* sig right)))
    (foreach-frame
      (incf (audio-out out1) (* sig left)))))

(define-ugen buffer-play* frame
    ((buffer buffer) rate start end)  
  (with-samples ((buf-rate (buffer-sample-rate buffer)))    
      (with ((frm (make-frame (block-size)))
             (mainpt (line* (* start buf-rate)
                            (*  end buf-rate)
                            (/ (- end start) rate)
                            #'free)))
        (maybe-expand mainpt)
        (foreach-frame
          (setf (frame-ref frm current-frame)
                (buffer-read buffer (frame-ref mainpt current-frame))))
        frm)))


(dsp! play-buffer* ((buffer buffer) (env incudine.vug:envelope) amp rate start end (out fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 1 0 0 0)
  (with-samples ((ampl (db->linear amp)))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) (buffer-sample-rate buffer))
                             end)))
      (with (
             (frm1 (envelope* env 1 (- ende start) #'free))
             (frm2 (buffer-play* buffer rate start ende))
             )
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (incf (audio-out out)
                (* ampl (frame-ref frm1 current-frame)
                   (frame-ref frm2 current-frame))))))))

