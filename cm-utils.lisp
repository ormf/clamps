;;; cm-utils.lisp
;;;
;;; Copyright (c) 2017 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package #:cm-utils)

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

(defmacro rt-sub (&rest rest)
  (alexandria:with-gensyms (fn)
    `(let ((,fn (eval ,@rest)))
       (loop
          while (funcall ,fn) 
          do (yield t)))))

