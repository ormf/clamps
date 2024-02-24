;;;; package.lisp

(in-package :cm)

;;; (shadowing-import '(play-sfz-one-shot play-sfz-loop) 'cl-sfz)
;; (shadowing-import '(at now) 'cm)
(shadowing-import '(trigger) 'cl-refs)

(shadow '(quantize at now tuning *tempo* buffer buffer-frames
          buffer-sample-rate
          lsample envelope
          lsample-keynum lsample-play-fn lsample-amp lsample-buffer
          lsample-buffer remove-all-responders recv-stop))
(shadowing-import '(node rescale-envelope
                    init stop bus group tempo
                    control-value timestamp
;;; play-sample

                    )
                  'incudine)
(use-package '(
               #:incudine
               #:of-incudine-dsps
               #:incudine-bufs
               #:cl #:cl-refs
               #:cl-sfz
               ;;; #:clog-dsp-widgets
               ))

