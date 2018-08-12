;;;; package.lisp

(defpackage #:of-incudine-dsps
  (:use #:cl #:ou #:incudine :incudine.vug :incudine.util :incudine.analysis)
  (:nicknames #:oid)
  (:shadowing-import-from #:incudine #:GROUP)
  (:export #:BUFFER-STRETCH-PLAY
           #:*HANNING1024*
           #:*ENV1*))

