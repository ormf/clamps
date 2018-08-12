;;;; package.lisp

(defpackage #:of-incudine-dsps
  (:use #:cl #:ou #:incudine :incudine.vug :incudine.util :incudine.analysis)
  (:shadowing-import-from #:incudine #:GROUP)
  (:export #:BUFFER-STRETCH-PLAY))

