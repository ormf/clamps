;;;; cm-utils.lisp

(in-package #:cm-utils)

;;; "cm-utils" goes here. Hacks and glory await!

(defmacro rt-wait (time &optional (yield t))
  `(list (cm:wait ,time)
         (yield ,yield)))

(defmacro rt-proc (&body body)
  (alexandria:with-gensyms (name)
    `(progn
       (defcoroutine ,name () ,@body)
       (make-coroutine ',name))))
