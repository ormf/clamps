;;;; package.lisp

(in-package :cm)

(shadowing-import '(play-sfz-one-shot play-sfz-loop) 'cl-sfz)
(use-package '(#:cl #:cl-refs #:clog-dsp-widgets #:incudine-bufs #:cl-sfz))
