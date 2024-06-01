;;;; package.lisp

(in-package :cl-user)

(defpackage #:cl-sfz
  (:use #:cl #:cl-ppcre #:incudine #:incudine-bufs)
;;;  (:shadowing-import-from #:incudine :sample-play :lsample-play)
  (:export
   #:load-sfz-preset
   #:list-sfz-presets
   #:remove-sfz-preset
   #:ensure-sfz-preset
   #:get-sfz-preset
   #:sfz-get-range
   #:sf-table-get-range
   #:play-sfz
   #:play-sfz-loop
   #:play-sfz-one-shot))

(in-package :of-incudine-dsps)

(export '(sfz->lsample get-keynum abs-path) 'of-incudine-dsps)
