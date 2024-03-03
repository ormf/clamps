;;;; package.lisp

(in-package :cm)

;;; (shadowing-import '(play-sfz-one-shot play-sfz-loop) 'cl-sfz)
;; (shadowing-import '(at now) 'cm)
(shadowing-import '(trigger) 'cl-refs)

(shadowing-import '(rescale-envelope
                    init stop bus group tempo
                    control-value timestamp
;;; play-sample
                    )
                  'incudine)

(shadow '(quantize at now tuning *tempo* buffer buffer-frames
          buffer-sample-rate
          node
          lsample envelope
          lsample-keynum lsample-play-fn lsample-amp lsample-buffer
          lsample-buffer remove-all-responders recv-stop
          cycle))
(shadowing-import '(*midi-in1* *midi-out1*
                    chan id)
                  'cl-midictl)


(use-package '(
               #:incudine
               #:cl-midictl
               #:of-incudine-dsps
               #:incudine-bufs
               #:cl #:cl-refs
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

(export '(reinit-midi restart-qsynth jack-connect-qsynth
          *mt-out01* *midi-in1* *midi-out1*
          start-cm-all)
        'cm)

