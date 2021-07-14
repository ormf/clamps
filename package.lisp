;;;; package.lisp

(defpackage #:cl-poolplayer
  (:shadowing-import-from #:cm :range :cd :pwd)
  (:use #:cl #:orm-utils #:cm #:of-incudine-dsps)
  (:export :dtime-dev :dtime :preset-play :eventplayer :r-elt
           :fig12-out))
