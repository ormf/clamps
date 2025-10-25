;;;; package.lisp

(in-package :cl-user)

(defpackage #:cl-sfz
  (:use #:cl #:cl-ppcre #:incudine #:incudine-bufs #:of-incudine-dsps)
;;;  (:shadowing-import-from #:incudine :sample-play :lsample-play)
  (:shadowing-import-from :orm-utils #:path-find-file)
  (:export
   #:add-sfz-preset
   #:load-sfz-preset
   #:list-sfz-presets
   #:remove-sfz-preset
   #:ensure-sfz-preset
   #:get-sfz-preset
   #:sfz-preset-buffers
   #:sfz-preset-lsamples
   #:sfz-preset-file
   #:sfz-get-range
   #:sfz-preset-loaded?
   #:sf-table-get-range
   #:play-sfz
   #:play-sfz-loop
   #:play-sfz-one-shot
   #:sfz->lsample
   #:get-keynum
   #:abs-path
   #:write-sfz
   ))

