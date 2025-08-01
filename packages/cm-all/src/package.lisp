;;;; package.lisp

(in-package :cm)

;;; (shadowing-import '(play-sfz-one-shot play-sfz-loop) 'cl-sfz)
;; (shadowing-import '(at now) 'cm)

(shadowing-import '(rescale-envelope
                    init stop group tempo
                    control-value timestamp
                    responder bpm
;;; play-sample
                    )
                  'incudine)

(shadowing-import '(clip)
                  'of-incudine-dsps)

(shadow '(quantize at now tuning *tempo* buffer buffer-frames
          buffer-sample-rate
          node bus
          lsample envelope
          lsample-keynum lsample-oneshot lsample-amp lsample-buffer
          lsample-buffer remove-all-responders recv-stop
          cycle without-interrupts
          midi-output
          trigger
          ))
(shadowing-import '(*midi-in1* *midi-out1*
                    chan id)
                  'cl-midictl)

(use-package '(
               #:cl-refs
               #:incudine
               #:cl-midictl
               #:incudine-bufs
               #:of-incudine-dsps
               #:cl
               #:cl-sfz
               #:clog-dsp-widgets
               #:ats-cuda
               #:ats-cuda-display
               #:clog-midi-controller
               ))


;;; weird voodoo as importing incudine:node gives the error that a
;;; symbol node already exists in cm, although it's not found/bound.
;;; We solve the problem by shadowing the symbol (see above) to be
;;; able to :use #:incudine, define it below and set the function
;;; definition of incudine:node to the bound symbol. In case we want
;;; to access the struct called node in incudine, we have to use
;;; incudine:node.

(defvar node nil)
(setf (fdefinition 'node) #'incudine:node)
(defvar bus nil)
(setf (fdefinition 'bus) #'incudine:bus)
(defsetf bus incudine::set-bus)

(defmacro imsg (type format-control &rest format-arguments)
  "Produce a formatted log message controlled by FORMAT-CONTROL and
FORMAT-ARGUMENTS.

TYPE should be one of ERROR, WARN, INFO or DEBUG."
  `(incudine.util::%msg ',(incudine::ensure-symbol type "INCUDINE.UTIL")
         ,format-control (list ,@format-arguments)))

(export '(reinit-midi restart-qsynth jack-connect-qsynth
          *mt-out01* *midi-in1* *midi-out1*
          start-cm-all cm-restart-gui imsg reset-logger-stream
          *sly-connected-hooks* call-sly-connected-hooks
          install-standard-sly-hooks)
        'cm)

