(in-package :of-incudine-dsps)


(define-vug ilag (in time)
  "Scaled one pole filter with the coefficient calculated from
a 60 dB lag TIME, intialized with the first received in value."
  (pole* in (delay1 (t60->pole time))))

(defun db->lin (value)
  "Convert VALUE dB to linear value."
  (expt (sample 10) (* value (sample 0.05))))

(declaim (inline keynum->hz))
(defun keynum->hz (keynum)
  "Convert Midicent /keynum/ to Hz, taking *​standard-pitch​* into
account. Returns Hz value as double float.

@Arguments
keynum - Number in Micents.

@See-also
standard-pitch
"
  (* (sample *standard-pitch*) (expt 2 (/ (- keynum 69.0d0) 12.0d0))))

(define-vug stereo (in) (out in in))

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

;; block-size = 1 ugens and dsps:

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

(dsp! play-buffer-stretch ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth)
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137)
  (with-samples ((rate (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                          (* *sample-rate* 8.175798915643707d0)))
                 (ampl (db->lin (ilag amp 0.1))))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope env 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (stereo sig))))


(dsp! play-buffer-stretch-out ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             *env1*
             0 0 0 0 1 137 0)
  (with-samples ((rate (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                          (* *sample-rate* 8.175798915643707d0)))
                 (ampl (db->lin (ilag amp 0.1))))
    (with-samples ((ende (if (or (zerop end)
                                 (> end (/ (buffer-frames buffer) *sample-rate*)))
                             (/ (buffer-frames buffer) *sample-rate*)
                             end))
                   (sig (* ampl
                           (envelope env 1 (* stretch (- ende start)) #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (incf (audio-out out) sig))))


(dsp! play-buffer-stretch-env-out ((buffer buffer) amp transp start end stretch wwidth attack release (out integer))
  (:defaults (incudine:incudine-missing-arg "BUFFER") 0 0 0 0 1 137 0 0.01 0)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->lin (ilag amp 0.1))))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             (min (/ (buffer-frames buffer) *sample-rate*) end)))
                   (sig (* (envelope (reduce-warnings (make-fasr attack ampl release (* stretch (- ende start))))
                                     1 1 #'free)
                           (buffer-stretch-play buffer rate wwidth start ende stretch))))
      (incf (audio-out out) sig))))

;;; (format t "~a , ~a ~a ~a ~a ~a ~a~%" (* stretch (- ende start)) buffer rate wwidth start ende stretch)

(dsp! play-buffer-stretch-env-pan-out ((buffer buffer) amp transp start end stretch wwidth attack release pan (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             0 0 0 0 1 137 0 0.01 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings
                         (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                            (* *sample-rate* 8.175798915643707d0))))
                 (ampl (db->lin (ilag amp 0.1)))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end)))
                 (sig (* (envelope (reduce-warnings (make-fasr attack ampl release (* stretch (- ende start))))
                                   1 1 #'free)
                         (buffer-stretch-play buffer rate wwidth start ende stretch))))
    (foreach-frame
      (incf (audio-out out2) (* sig right))
      (incf (audio-out out1) (* sig left)))))


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

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

(define-ugen phasor-loop* frame (rate start-pos loopstart loopend)
  "Ugen of a looping phasor working with any block size.
@Arguments
rate - Number denoting sample increment.
start-pos - Number denoting initial position.
loopstart - Non Negative Integer denoting start of loop.
loopend - Non Negative Integer denoting end of loop.

@See-also
buffer-loop-play*
buffer-play*
buffer-stretch-play*
envelope*
line*
phasor*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
"  (with ((frm (make-frame (block-size))))
    (foreach-frame
      (setf (frame-ref frm current-frame)
            (phasor-loop rate start-pos loopstart loopend)))
    frm))


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


#|

(defparameter *buf* (incudine:buffer-load
 "~/work/kompositionen/big-orchestra/snd/bo-samples/aitken-mallet/einzeln-gsp/T03_1-239.wav"))

(rt-start)
(at (+ (now) (* *sample-rate* 1)) #'play-buffer-stretch* :buffer *buf* :end 0.2)
(play-buffer-stretch* :buffer *buf* :end 0.2)
(incudine::node-free-all)

|#


;;; (defparameter *my-buf* (buffer-load "/home/orm/work/kompositionen/letzte-worte/snd/fl-s01-line01.wav"))

;; (play-buffer-stretch-out :buffer *my-buf* :amp 0 :transp 0 :start 0 :end 0 :stretch 1 :wwidth 137 :out 1)
;; (play-buffer-stretch :buffer *my-buf* :amp 0 :transp -12 :start 0 :end 0 :stretch 1 :wwidth 137)

(define-ugen buffer-play* frame ((buffer buffer) rate startframe endframe)
    "Buffer play Ugen working with any blocksize.

@Arguments
buffer - Incudine buffer.
rate - Positive Number denoting playback rate (will get adjusted to buffer-sample-rate).
start-frame - Non Negative Integer denoting start position of playback in samples.
endframe - Non Negative Integer denoting end position of playback in samples.

@See-also
buffer-loop-play*
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
amp - Positive Number denoting amplitude in dB. The dB range [-100..0] is mapped to the linear range [0..1].
rate - Positive Number denoting playback rate, adjusted for the sample rate of buffer.
pan - Number in the range [0..1] denoting panorama between out1 and out2.
startpos - Positive Number denoting in seconds into the buffer.
out1 - Non Negative Integer denoting first output channel.
out2 - Non Negative Integer denoting second output channel.

@See-also
buffer-play*
buffer-loop-play*
buffer-stretch-play*
envelope*
line*
lsample
phasor*
phasor-loop*
play-buffer-loop*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
play-lsample
"
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 1)
  (with-samples (;;; (rate (* (/ (buffer-sample-rate buffer) *sample-rate*) rate))
                 (startframe (* startpos (buffer-sample-rate buffer)))
                 (ampl (db->lin (ilag amp 0.1)))                 
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

(define-ugen buffer-loop-play* frame ((buffer buffer) rate start-pos
                                      loopstart loopend)
    "Buffer loop play Ugen working with any blocksize.

@Arguments
buffer - Incudine buffer.
rate - Positive Number denoting playback rate (will get adjusted to buffer-sample-rate).
start-pos - Non Negative Integer denoting start position in samples.
loopstart - Non Negative Integer denoting start of loop in samples.
loopend - Non Negative Integer denoting end of loop in samples.

@See-also
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
amp - Positive Number denoting amplitude in dB. The dB range [-100..0] is mapped to the linear range [0..1].
rate - Positive Number denoting playback rate, adjusted for the sample rate of buffer.
pan - Number in the range [0..1] denoting panorama between out1 and out2.
loopstart - Positive Number denoting start of loop.
loopstart - Positive Number denoting end of loop.
out1 - Non Negative Integer denoting first output channel.
out2 - Non Negative Integer denoting second output channel.

@See-also
buffer-play*
buffer-loop-play*
buffer-stretch-play*
envelope*
line*
lsample
phasor*
phasor-loop*
play-buffer*
play-buffer-stretch*
play-buffer-stretch-env-pan-out*
play-lsample
"
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 0 0 1)
  (with-samples ((startframe (* start (buffer-sample-rate buffer)))
                 (ampl (db->lin (ilag amp 0.1)))
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



#|
(dsp! play-sample* ((buffer buffer) (env incudine.vug:envelope) dur amp rate pan start (out1 fixnum) (out2 fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             1 0 1 0.5 0 0 1)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (ampl (db->lin amp))
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
  "Buffer play Ugen with granular stretching working with any blocksize.

@Arguments
buffer - Incudine buffer.
rate - Positive Number denoting playback rate (will get adjusted to buffer-sample-rate).
wwidth - Non Negative Integer denoting granular window size in msecs.
start - Non Negative Integer denoting start of playback into the buffer in samples.
end - Non Negative Integer denoting end of playback into the buffer in samples.
stretch - Positive Integer denoting stretching factor.

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
"  (with-samples ((rate (* rate (/ (buffer-sample-rate buffer) *sample-rate*)))
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
                     (buffer-read buffer (+ (samphold mpt p1) (* p1 (samphold wsamps p1)))))
                  (* (buffer-read *hanning1024* (* p2 1024))
                     (buffer-read buffer
                                  (max 0.0d0
                                       (+
                                        (samphold mpt p2 (* -0.5 wsamps) -1)
                                        (* p2 (samphold wsamps p2))))))))))
       frm)))

(dsp! play-buffer-stretch* ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth (done-action function))
  "Play /buffer/ using 2 window overlap add granular
stretching/transposition with /env/ between /start/ and /end/, /amp/
in dB, transposition /transp/, /stretch/ and grain window size
/wwidth/ to output 0. All other keywords of incudine dsps also
apply. Works with any block size.

@Arguments
buffer - Incudine Buffer.
env - Incudine Envelope.
amp - Positive Number denoting amplitude in dB. The dB range [-100..0] is mapped to the linear range [0..1].
transp - Number denoting transposition in Midicents.
start - Number denoting the start in seconds into the buffer (at buffer rate).
end - Number denoting the end in seconds into the buffer (at buffer rate). Zero denotes end of buffer.
stretch - Positive Number denoting time stretching factor of playback.
wwidth - Positive Number denoting granular window width in msecs.

@See-also
buffer-play*
buffer-loop-play*
buffer-stretch-play*
envelope*
line*
lsample
phasor*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch-env-pan-out*
play-lsample
"
  (:defaults (incudine:incudine-missing-arg "BUFFER")
             (incudine:incudine-missing-arg "ENV")
             0 0 0 0 1 137 #'free)
  (with-samples ((rate (reduce-warnings (/ (keynum->hz transp)
                                           8.175798915643707d0)))
                 (ampl (db->lin (ilag amp 0.1))))
    (with-samples ((ende (if (zerop end)
                             (/ (buffer-frames buffer) *sample-rate*)
                             end)))
      (with (
             (frm1 (envelope* env 1 (* stretch (- ende start)) done-action))
             (frm2 (buffer-stretch-play* buffer rate wwidth start ende stretch))
             )
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (stereo (* ampl (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))))))

(dsp! play-buffer-stretch-env-pan-out*
    ((buffer buffer) (env incudine.vug:envelope) amp transp start end stretch wwidth attack release pan (out1 fixnum) (out2 fixnum) (done-action function))
  "Play /buffer/ using 2 window overlap add granular
stretching/transposition with /env/ between /start/ and /end/, /amp/
in dB, transposition /transp/, /stretch/ and grain window size
/wwidth/, /attack/ and /release/. Output will be panned between /out1/
and /out2/. All other keywords of incudine dsps also apply. Works with
any block size.

@Arguments
buffer - Incudine Buffer.
env - Incudine Envelope.
amp - Positive Number denoting amplitude in dB. The dB range [-100..0] is mapped to the linear range [0..1].
transp - Number denoting transposition in Midicents.
start - Number denoting the start in seconds into the buffer (at buffer rate).
end - Number denoting the end in seconds into the buffer (at buffer rate). Zero denotes end of buffer.
stretch - Positive Number denoting time stretching factor of playback.
wwidth - Positive Number denoting granular window width in msecs.
attack - Positive Number in the range [0..1] denoting attack time as a fraction of the duration.
release - Positive Number in the range [0..1] denoting release time as a fraction of the duration.
pan - Number in the range [0..1] denoting panorama between out1 and out2.
out1 - Non Negative Integer denoting first output channel. Defaults to 0.
out2 - Non Negative Integer denoting second output channel. Defaults to (1+ out1).

@See-also
buffer-play*
buffer-loop-play*
buffer-stretch-play*
envelope*
line*
lsample
phasor*
phasor-loop*
play-buffer*
play-buffer-loop*
play-buffer-stretch*
play-lsample
"  (:defaults (incudine:incudine-missing-arg "BUFFER")
             *env1*
             0 0 0 0 1 137 0 0.01 0 0 1 #'free)
  (with-samples ((alpha (* +half-pi+ pan))
                 (left (cos alpha))
                 (right (sin alpha))
                 (rate (reduce-warnings (/ (* (buffer-sample-rate buffer) (keynum->hz transp))
                                           (* *sample-rate* 8.175798915643707d0))))
                 (ampl (db->lin (ilag amp 0.1)))
                 (ende (if (zerop end)
                           (/ (buffer-frames buffer) *sample-rate*)
                           (min (/ (buffer-frames buffer) *sample-rate*) end))))
    (with ((frm1 (envelope* env 1 (* stretch (- ende start)) done-action))
           (frm2 (buffer-stretch-play* buffer rate wwidth start ende stretch)))
        (maybe-expand frm1)
        (maybe-expand frm2)
        (foreach-frame
          (let ((sig (* ampl
                     (frame-ref frm1 current-frame)
                     (frame-ref frm2 current-frame))))
            (incf (audio-out out2) (* sig right))
            (incf (audio-out out1) (* sig left)))))))

