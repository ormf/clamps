;;;; package.lisp

(defpackage #:cm-utils
  (:shadowing-import-from #:cm
                          :at :now :tuning :*tempo* :play :rescale-envelope :quantize :stop :group :range)
  (:use #:cl #:incudine #:cm #:orm-utils #:cl-coroutine))

