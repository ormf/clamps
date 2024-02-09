;;;; cl-refs.lisp
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       reactive lib                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-refs)

(defparameter *debug* nil)
(defparameter *ref-id* '(0))

(defun next-id (ref-id)
  "Generate unique ids for use in scripts."
  (atomics:atomic-incf (car ref-id)))

(defun get-ref-id ()
  (format nil "ref~a" (next-id *ref-id*)))

(defparameter *update-deps* nil)
;;; (defparameter *update-deps* t)
(defparameter *curr-ref* nil)
(defparameter *curr-callback* nil)
(defparameter *refs-seen* nil) ;;; keep track of already set refs to
                              ;;; avoid duplicate recalculation and
                              ;;; handle circular dependencies
                              ;;; elegantly.

;;; this is the core object

(defclass ref-object-super ()
  ((id :initform (get-ref-id) :reader ref-id)
   (listeners :initarg :listeners :initform '() :accessor ref-listeners)
   (dependencies :initarg :dependencies :initform '() :accessor ref-dependencies)))

(defclass bang-object (ref-object-super)
  ((value :initarg :value :initform nil :reader ref-value)))

(defclass ref-object (ref-object-super)
  ((value :initarg :value :accessor ref-value)
   (setter :initarg :setter :initform '() :reader ref-setter)
   (fun :initarg :fun  :initform '() :reader ref-fun)
   (update :initarg :update  :initform '() :accessor ref-update)))

(defmethod print-object ((obj ref-object) stream)
  (format stream "#<ref ~a>" (ref-value obj)))

;;; constructor
(defun make-ref (val &rest args)
  (apply #'make-instance 'ref-object :value val args))

;;; this is the setter. It updates the value and calls all listeners.
(defun %set-val (ref val)
  "don't use this directly in the top-level, rather use set-val.
%set-val sets the ref to val and triggers the computation of related
objects in the context of the dynamic variable *refs-seen*, which
tracks the references already updated to avoid unnecessary
recalcuations and problems with circular dependencies."
  (let ((old (ref-value ref)) (setter (ref-setter ref)))
    (unless (equal val old) ;;; unless is the opposite of when; when/unless
;;; don't need progn, because ther is no
;;; else clause.
;;;      (if *debug* (format t "~&%set-val called: ~a~%" (obj-print *refs-seen*)))
      (if setter
          (progn
            (push setter *refs-seen*)
            (funcall setter val)) ; update val if fun is not defined? Not sure/needed yet
          (setf (ref-value ref) val))
      (dolist (listener (ref-listeners ref))
        (unless (member listener *refs-seen*)
          (push listener *refs-seen*)
;;;          (format t "%set-val: ~a, val: ~a, refs-seen: ~a" listener val *refs-seen*)
          (funcall listener old val)))))
  (ref-value ref))

(defun set-val (ref value)
  "set the value of ref in the context of a freshly cleared *refs-seen*."
  (let ((*refs-seen* nil))
    (%set-val ref value))
  (setf *refs-seen* nil) ;;; just to make sure that the global variable is cleared (not really necessary).
  value)

;;; this is a magic getter. It tracks the access and adds the accessor
;;; to the listeners. This is very cool.

(defun get-val (ref)
  "get the value of ref, updating listeners and dependencies of the
listener's ref if *curr-ref* is set in the dynamic scope of the call
to get-val (see #'make-computed and #'watch)."
  (when *curr-ref*
    (pushnew *curr-callback* (ref-listeners ref))
    (pushnew ref (ref-dependencies *curr-ref*)))
  (ref-value ref))

(defmethod print-object ((obj bang-object) stream)
  (format stream "#<bang>"))

(defun make-bang (&optional fn)
  (make-instance 'bang-object :listeners (if fn (list fn))))

(defun %trigger (obj)
  (map nil #'funcall (ref-listeners obj)))

(defgeneric trigger (obj)
  (:method ((obj bang-object))
    (let ((*refs-seen* nil)) (map nil #'funcall (ref-listeners obj)))))

(defun clear-dependencies (co cb)
  "clear all dependencies of a computed ref object."
  (dolist (dep (ref-dependencies co))
    (setf (ref-listeners dep) (remove cb (ref-listeners dep))))
  (setf (ref-dependencies co) '()))

(defmacro with-updating-deps (&body body)
  "evaluate body in the dynamic context of *update-defs* set to T"
  `(let ((*update-deps* t))
     ,@body))

(defmacro on-deps-update (&rest body)
  "return body if *update-deps* is non-nil, otherwise return nil."
  `(when *update-deps* ,@body))
 
#|
(defmacro with-unwatched (bindings &body body)
  "all #'get-val forms contained in bindings are not watched."
  (let ((tempvar (gensym)))
    `(let ((,tempvar cl-refs::*curr-ref*))
       (setf cl-refs::*curr-ref* nil)
       (let* ,bindings
         (setf cl-refs::*curr-ref* ,tempvar)
         ,@body))))
|#

(defmacro with-unwatched (bindings &body body)
  "all #'get-val forms contained in bindings are not watched."
  (let ((tempvar (gensym)))
    `(let* ((,tempvar cl-refs::*curr-ref*)
            (cl-refs::*curr-ref* nil)
            ,@bindings
            (cl-refs::*curr-ref* ,tempvar))
       ,@body)))


;;; this is another constructor but instead of a value we can add a function.
;;; Together with the getter it tracks which object was accessed and adds it to the dependencies.

(defun make-computed (fn &optional (setter nil))
  "define/create a ref variable using fn to calculate its value with
automagic update whenever any ref value in fn changes."
;;; let* is sequential to avoid nesting of multiple let
  (let* ((new-ref (make-ref nil :fun fn :setter setter))
         (update-callback (lambda (&optional old new) (declare (ignorable old new)) (funcall (ref-update new-ref)))))
    (with-updating-deps
      (setf (ref-update new-ref)
            (lambda () ;;; this update function is closed over new-ref and update-callback
              (on-deps-update (clear-dependencies new-ref update-callback))
              (let ((old (ref-value new-ref))) ;;; memorize last value
                (let ((*curr-ref* (on-deps-update new-ref))
                      (*curr-callback* update-callback))
;;; update dependencies and store new value without using setter.
;;;
;;; the funcall of fn (the first argument of computed) is closed over
;;; dynamic bindings of new-ref and update-callback (being a funcall
;;; of this update fn); if fn executes any get-val functions, their
;;; ref gets pushed into the ref-dependencies of new-ref and the
;;; update-callback gets pushed into the listeners of the get-val
;;; ref. This update-callback will therefore get called whenver the
;;; ref is changed using set-ref.
                  (setf (ref-value new-ref) (funcall fn)))
                (let ((*curr-ref* nil)
                      (*curr-callback* nil))
;;; we have to update listeners manually as we did not use %set-val to
;;; set the ref-value three lines above.
                  (unless (equal (ref-value new-ref) old)
                    (dolist (listener (ref-listeners new-ref))
                      (unless (member listener *refs-seen*)
                        (push listener *refs-seen*)
                        (funcall listener old (ref-value new-ref)))))))
              new-ref))
      ;; call the update function once to register all callbacks.
      (funcall (ref-update new-ref)))))

;;; watch is similar to make-computed. In contrast to make-computed it
;;; mainly implements behaviour. Like computed it uses a ref-object
;;; internally but its main purpose is to trigger behaviour by calling
;;; its supplied f, whenever reference values accessed within that
;;; function change. Watch returns its cleanup function, removing the
;;; ref cell and its listeners.
;;;
;;; Note: This function doesn't return the new ref-object, but an
;;; unwatch function to remove the ref-object and all its
;;; dependencies.

(defun watch (f)
  (let* ((new-ref (make-ref nil :fun f))
         (update-callback (lambda (&optional old new)
                            (declare (ignorable old new))
                            (funcall (ref-update new-ref)))))
    (with-updating-deps
      (setf (ref-update new-ref)
            (lambda ()
              (on-deps-update (clear-dependencies new-ref update-callback))
              ;; the let below establishes a dynamic context for the
              ;; funcall in its body. If *curr-ref* is non-nil, any
              ;; #'get-val access to a ref in ref-fun will push the
              ;; update-callback to the listeners of the ref and the
              ;; ref to the dependencies of the new-ref. In the first
              ;; call to watch, *update-deps* is set to T to ensure
              ;; all dependencies/listeners get registered. Subsequent
              ;; calls to ref-update depend on the value of
              ;; *update-deps* in the dynamic context in which they
              ;; are called to avoid unnecessary duplicate registering
              ;; of the dependencies/listeners on each update
              ;; triggered by value changes in the observed refs.
              (let ((*curr-ref* (on-deps-update new-ref)) 
                    (*curr-callback* update-callback))
                ;; Note: storing the new value doesn't seem to make sense as the
                ;; object's value isn't supposed to be read anywhere. Watch is rather
                ;; used for its side effects only.
                (setf (ref-value new-ref) (funcall (ref-fun new-ref))))
              new-ref))
      ;; call the update function once to register a call to it in all
      ;; ref-objects read in <f>.
      (funcall (ref-update new-ref)))
    (lambda () ;;; return the unwatch/cleanup fn
      (clear-dependencies new-ref update-callback)
      (makunbound 'new-ref))))
#|
(defun remove-watch (ref)
  (when (ref-cleanup ref) (funcall (ref-cleanup ref))))
|#

;;; just a helper function. I heard you like to copy variables. This is how to copy a ref:
(defun copy-ref (ref)
  (make-computed (lambda () (get-val ref))
                 (lambda (val) (%set-val ref val))))
