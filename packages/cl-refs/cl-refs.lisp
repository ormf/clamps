;;;; cl-refs.lisp
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       reactive lib                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:cl-refs)

;;; (defparameter *debug* nil)
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

(defclass ref-object (ref-object-super)
  ((value :initarg :value :accessor ref-value)
   (setter :initarg :setter :initform '() :reader ref-setter)
   (fn :initarg :fn  :initform '() :reader ref-fn)
   (update :initarg :update  :initform '() :accessor ref-update)
   )
  (:documentation
   "A /ref-object/ is a special class used in the /cl-refs/
package. Its slots shouldn't be accessed or manipulated directly,
but rather using the public functions of the cl-refs package listed
below. A ref-object should get instantiated using the <<make-ref>> function.

For information how to use ref-objects refer to <<clamps:cl-refs>> in
the Clamps Packages documentation.

@See-also
bang-object
get-val
make-bang
make-computed
make-ref
set-val
watch
"))

(defmethod print-object ((obj ref-object) stream)
  (format stream "#<ref ~S>" (ref-value obj)))

(defclass bang-object (ref-object)
  ((trigger-fns :initform nil :initarg :trigger-fns :accessor trigger-fns))
    (:documentation
   "A /bang-object/ is a /ref-object/ with an additional =trigger-fns=
slot which get called when the object is the argument of the trigger
function. Apart from that it behaves like a ref-object. A bang object
should get instantiated using the <<make-bang>> function.

@See-also
get-val
make-bang
make-computed
make-ref
ref-object
set-val
watch
"))

(defmethod print-object ((obj bang-object) stream)
  (format stream "#<bang ~S>" (ref-value obj)))

;;; constructor
(defun make-ref (val &rest args)
  "Return an instance of <<ref-object>> with initial value /val/.

@Arguments
val - Initial value of the created instance. It can be of any
type.

args - Optional args supplied to make-instance. They are used
internally and are not intended to be used directly when working
with /cl-refs/.

@See-also
bang-object
get-val
make-computed
ref-object
set-val
watch
"  (apply #'make-instance 'ref-object :value val args))

;;; this is the setter. It updates the value and calls all listeners.
(defun %set-val (ref val &key (force nil))
  "don't use this directly in the top-level, rather use set-val.
%set-val sets the ref to val and triggers the computation of related
objects in the context of the dynamic variable *refs-seen*, which
tracks the references already updated to avoid unnecessary
recalcuations and problems with circular dependencies."
  (let ((old (ref-value ref)) (setter (ref-setter ref)))
    (when (or force (not (equal val old))) ;;; 
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

(defun set-val (ref value &key (force nil))
  "Set the value of ref-object /ref/ to /value/ if different than
previous value. If /force/ is non-nil, set in any case. Return
value.

@Arguments
ref - An instance of <<ref-object>>
value - Any value of any type to be set.
force - A boolean indicating to set the value even if it is eql to
the previous value of the ref-object.

@See-also
bang-object
get-val
make-computed
make-ref
ref-object
watch
"
  (let ((*refs-seen* nil))
    (%set-val ref value :force force))
  (setf *refs-seen* nil) ;;; just to make sure that the global variable is cleared (not really necessary).
  value)

;;; this is a magic getter. It tracks the access and adds the accessor
;;; to the listeners. This is very cool.

(defun get-val (ref)
  "Return the value of /ref/.

@Arguments
ref - An instance of <<ref-object>>.

@See-also
bang-object
make-computed
make-ref
ref-object
set-val
watch
"
  (when *curr-ref*
    (pushnew *curr-callback* (ref-listeners ref))
    (pushnew ref (ref-dependencies *curr-ref*)))
  (ref-value ref))

(defun getter (ref)
  "more generic than get-val: Get the value of /ref/ using an appropriate
getter fn depending on type of ref.

@Arguments
ref - An instance of <<ref-object>> or anything can be called on
value - Any value of any type to be set.
force - A boolean indicating to set the value even if it is eql to
the previous value of the ref-object.

@See-also
bang-object
getter
get-val
make-computed
make-ref
ref-object
set-val
watch
"
  (typecase ref
    (ref-object (get-val ref))
    (otherwise ref)))


(defun make-bang (&optional fn val)
  "create and return a <<bang-object>> instance with trigger-fns set to /fn/
and its ref-value set to /val/.

@Arguments

fn - A function or list of functions to call on the trigger function.
val - The value of the ref-object.

@Example

(defparameter *test-bang* (make-bang (lambda () (format t \"~&HiHo\")) 2.5))

(trigger *test-bang*) ;;; -> nil
;;; output in the REPL: HiHo

(get-val *test-bang*) ;;; -> 2.5

(set-val *test-bang* 42) ;;; -> 42

(get-val *test-bang*) ;;; -> 42

@See-also
bang-object
get-val
ref-object
set-val
trigger
"
  (make-instance 'bang-object :trigger-fns (if (functionp fn) (list fn) fn)
                 :value val))

(defun %trigger (obj)
  "call all trigger-fns of /obj/ in the dynamic scope of *refs-seen*."
  (map nil #'funcall (trigger-fns obj)))

(defgeneric trigger (obj)
  (:documentation "call all trigger-fns of /obj/ with *refs-seen* set to nil.")
  (:method ((obj bang-object))
    (let ((*refs-seen* nil)) (%trigger obj))))

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

(defmacro setter (ref value &key (force nil))
  "more generic than set-val: Set the value of /ref/ to
/value/ using an appropriate setter fn depending on type of ref

@Arguments
ref - An instance of <<ref-object>> or anything setf can be called on
value - Any value of any type to be set.
force - A boolean indicating to set the value even if it is eql to
the previous value of the ref-object.

@See-also
bang-object
getter
get-val
make-computed
make-ref
ref-object
set-val
watch
"
  `(typecase ,ref
     (ref-object (set-val ,ref ,value :force ,force))
     (otherwise (setf ,ref ,value))))

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
  "Return a <<ref-object>> which recalculates and sets its value using
/fn/ whenever a ref-object accessed with <<get-val>> in the body of
/fn/ is changed.

Refer to <<clamps:Defining relations>> in the Clamps documentation for
examples.

@Arguments
fn - Function of no arguments to call whenever a value accessed
using <<get-val>> in the body of the function is changed.

setter - Function of one argument called with the value of the
ref-object returned by /make-computed/ whenever it changes.

@See-also
get-val
make-ref
set-val
watch
"
;;; let* is sequential to avoid nesting of multiple let
  (let* ((new-ref (make-ref nil :fn fn :setter setter))
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
      (funcall (ref-update new-ref))
      new-ref)))

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

(defun watch (fn)
  "Call /fn/ whenever a value accessed using <<get-val>> in the body of
the function is changed.

/watch/ returns a function to remove the relation, /watch/ has
established. Refer to the chapter <<clamps:cl-refs>> in the Clamps
Packages documentation for examples.

@Arguments
fn - Function of no arguments to call

@See-also
add-watch
get-val
make-computed
make-ref
set-val
unwatch-all
"  (let* ((new-ref (make-ref nil :fn fn))
          (update-callback (lambda (&optional old new)
                             (declare (ignorable old new))
                             (funcall (ref-update new-ref)))))
     (with-updating-deps
       (setf (ref-update new-ref)
             (lambda ()
               (on-deps-update (clear-dependencies new-ref update-callback))
               ;; the let below establishes a dynamic context for the
               ;; funcall in its body. If *curr-ref* is non-nil, any
               ;; #'get-val access to a ref in ref-fn will push the
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
                 (setf (ref-value new-ref) (funcall (ref-fn new-ref))))
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

(defmacro add-watch (unwatch &rest forms)
  "Add function bodies in /forms/ to /unwatch/.

@Arguments
unwatch - A list containing functions to undo a watch definition.
forms - Zero or more function bodies supplied to the #'watch function.

@Example

(defparameter *unwatch* nil)
(defparameter *v1* (make-ref 0.0))
(defparameter *v2* (make-ref 0.0))

(add-watch
 *unwatch*
 (format t \"~&v1 changed: ~a\" (get-val *v1*))
 (format t \"~&v2 changed: ~a\" (get-val *v2*)))
;; => (#<function (lambda () :in watch) {120DE2BB8B}>
;;     #<function (lambda () :in watch) {120DE2BAEB}>)
;;
;; Output in REPL:
;; v1 changed: 0.0
;; v2 changed: 0.0

(set-val *v1* 0)
;; => 20
;;
;; Output in REPL:
;; v1 changed: 0.0

(set-val *v2* -2.3)
;; => -2.3
;;
;; Output in REPL:
;; v2 changed: -2.3

(unwatch-all *unwatch*)

*unwatch* ; => nil

(set-val *v1* 0)
;; => 0
;;
;; No Output in REPL.

@See-also
unwatch-all
watch
"
  `(progn
     ,@(loop
         for form in forms
         collect (if unwatch
                     `(push (watch (lambda () ,form)) ,unwatch)
                     `(watch (lambda () ,form))))))


(defmacro unwatch-all (unwatch)
  "Call all functions of /unwatch/ and set /unwatch/ to nil.

@Arguments
unwatch - List of functions with no arguments removing a previously established watch association.

@See-also
add-watch
watch
"
  `(setf ,unwatch (map '() #'funcall ,unwatch)))

(defun add-trigger-fn (ref &rest fns)
  "Add one or more /fns/ to be executed when /ref/ is called with the
#'trigger function. The function returns a function removing the
trigger function(s) from ref, comparable to the <<watch>> function on
a <ref-object>>.

@Arguments
ref - A <<bang-object>>.
fns - one or more functions of zero argument called when the trigger function is invoked on ref.

@See-also
make-bang
remove-all-triggers
remove-trigger-fn
"
  (declare (type bang-object ref))
  (dolist (fn fns) (pushnew fn (trigger-fns ref)))
  (lambda () (dolist (fn fns) (remove-trigger-fn ref fn))))

(defun remove-all-triggers (ref &key (unless (lambda (elem) elem nil)))
  "Remove all trigger-fns of /ref/ which don't match the /unless/
predicate.

@Arguments
ref - A <<bang-object>>.
unless - Predicate called on all trigger-fns to determine which functions to keep.

@See-also
add-trigger-fn
remove-trigger-fn
make-bang
"
  (declare (type bang-object ref))
  (setf (trigger-fns ref) (remove-if-not unless (trigger-fns ref))))

(defun remove-trigger-fn (ref fn)
  "Remove /fn/ from the trigger-fns of /ref/.

@Arguments
ref - A <<bang-object>>.
unless - Predicate called on all trigger-fns to determine which functions to keep.

@See-also
add-trigger-fn
make-bang
remove-all-triggers
"
  (declare (type bang-object ref))
  (setf (trigger-fns ref) (remove fn (trigger-fns ref))))

(defun toggle-ref-fn (ref &optional (num-states 2))
  "Return a function of no arguments which cycles the values of ref-cell
/ref/ between 0 and (1- /num-states/).

@Arguments
ref - A <<ref-object>> or <<bang-object>>.
"
  (lambda () (set-val ref (mod (1+ (get-val ref)) num-states))))
