;;;; cm-utils.lisp

(in-package #:cm-utils)

;;; "cm-utils" goes here. Hacks and glory await!

(defparameter *local-time* 0)

(defmacro rt-wait (time &optional (yield t))
  `(progn
     (cm:wait ,time)
     (incf *local-time* ,time)
     (yield ,yield)))

(defmacro rt-sprout (s-expr &key (at))
  `(sprout ,s-expr :at (or ,at (now))))

(defmacro rt-proc (&body body)
  (alexandria:with-gensyms (name)
    `(progn
       (defcoroutine ,name () ,@body)
       (make-coroutine ',name))))

(defmacro subproc (&rest rest)
  (alexandria:with-gensyms (fn)
    `(let ((,fn (apply ,(first rest) ,(cdr rest))))
       (loop
          while (funcall ,fn) 
          do (yield t)))))

(defmacro subproc (&rest rest)
  (alexandria:with-gensyms (fn)
    `(let ((,fn (eval ,@rest)))
       (loop
          while (funcall ,fn) 
          do (yield t)))))
