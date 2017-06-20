;;;; cm-utils.lisp

(in-package #:cm-utils)

;;; "cm-utils" goes here. Hacks and glory await!
(rt-start)
(cm::midi-open-default :direction :output)
(setf cm::*rts-out* (new cm::incudine-stream))

(defun test-proc ()
  (rt-proc
    (loop
       for x below 30
       do (progn
            (output (new midi :keynum 60 :time (now) :duration 0.05))
            (rt-wait 0.1)))))


(sprout (test-proc))

(events (test-proc) "/tmp/test.midi" :play nil)
