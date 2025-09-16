;;;; clog-dsp-widgets.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl
;;; <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package #:clog-dsp-widgets)

;;; clog extension to integrate reactive with clog.
;;;
;;; A binding establishes a relation between a ref object <refvar> and
;;; an attribute name <attr> using #'bind-ref-to-attr. This function
;;; creates the binding and calls the watch function, which creates a
;;; ref object containing the refvar in its update fn. Adding a clog
;;; element to the b-elist of the binding will result in an update of
;;; the attribue whenever the state of the <refvar> object changes.
;;;
;;; Note: There is no handle to the update function created by watch,
;;; but the watch function returns an unwatch function which removes
;;; the ref object created in #'bind-ref-to-attr and all its
;;; dependencies. This function is stored in the unwatch slot of the
;;; binding to facilitate unbinding.

(defvar *bindings* (make-hash-table :test 'equal))

(defun clear-bindings ()
  (maphash (lambda (name binding) ;;; remove watch functions
             (declare (ignore name))
             (funcall (b-unwatch binding)))
           *bindings*)
  (setf *bindings* (make-hash-table :test 'equal)))

(defclass binding ()
  ((ref :initarg :ref :accessor b-ref)
   (attr :initarg :attr :accessor b-attr)
   (elist :initarg :elist :initform '() :accessor b-elist :documentation "bound html elements")
   (map :initarg :map :accessor b-map)
   (unwatch :initarg :unwatch :initform '() :accessor b-unwatch)))

(defun binding-name (refvar attr)
  (concatenate 'string (ref-id refvar) "-" attr))

(defun make-binding (&rest args)
  (apply #'make-instance 'binding args))

(defun bang-watch (refvar attr new)
  "special watch function for bang-objects: pushes a function triggering
a call to bang() on all elements of (b-elist new) to the trigger-fns
of refvar. refvar has to be a bang-object."
  (let ((fn (lambda () (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
                    (unless (member (list obj attr) *refs-seen* :test #'equal)
                      ;; (if *debug* (format t "~&~%watch update: ~a~%-> ~a~%" (obj-print *refs-seen*) obj))
                      (push obj *refs-seen*)
                      (if (equal attr "bang")
                          (js-execute obj (format nil "~A.bang()" (script-id obj))))
                      )))))
    (push fn (trigger-fns refvar))
    (lambda () (remove fn (trigger-fns refvar)))))

(defun get-attribute-form (val)
  "return the form to set in the attribute of an html element. Defaults
to val but in case of structs can also have other forms (see
clamps-sensors:sensors.lisp for a overwrite of this function)."
  val)

;;; (trigger x-bang)
(defgeneric define-watch (refvar attr new)
  (:method ((refvar ref-object) attr new)
    (watch ;;; watch registers in an on-update function
     (lambda ()
       (let ((val (get-val refvar)))
         ;; (if *debug* (format t "~&~%elist: ~a~%" (b-elist new)))
         ;; (if *debug* (format t "~&~%seen: ~a~%" (obj-print *refs-seen*)))
         (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
           (unless (member (list obj attr) *refs-seen* :test #'equal)
             ;; (if *debug* (format t "~&~%watch update: ~a~%-> ~a ~a~%" (obj-print *refs-seen*) obj val))
             (push (list obj attr) *refs-seen*)
             (setf (attribute obj attr) (get-attribute-form val))))))))
  (:method ((refvar bang-object) attr new)
    (if (equal attr "bang")
        (bang-watch refvar attr new)
            (watch ;;; watch registers in an on-update function
             (lambda ()
               (let ((val (get-val refvar))) ;; we read val only to register
                 ;; the watch function in the
                 ;; listeners of the bang
                 ;; (if *debug* (format t "~&~%elist: ~a~%" (b-elist new)))
                 ;; (if *debug* (format t "~&~%seen: ~a~%" (obj-print *refs-seen*)))
                 (dolist (obj (b-elist new)) ;;; iterate through all bound html elems
                   (unless (member (list obj attr) *refs-seen* :test #'equal)
                     ;; (if *debug* (format t "~&~%watch update: ~a~%-> ~a~%" (obj-print *refs-seen*) obj))
                     (push obj *refs-seen*)
                     (setf (attribute obj attr) val)))))))))

(defgeneric bind-ref-to-attr (refvar attr &optional map)
  (:method ((refvar ref-object-super) attr &optional (map (lambda (val) val)))
    (let ((name (binding-name refvar attr)))
      (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
          (let ((new (make-binding :ref refvar :attr attr :map map)))
            (setf (b-unwatch new) (define-watch refvar attr new))
            (setf (gethash name *bindings*) new)))))
  (:method ((refvar-array simple-array) attr &optional (map (lambda (val) val)))
    (let ((binding-array (make-array (length refvar-array))))
      (loop for refvar across refvar-array
            for idx from 0
            do (setf (aref binding-array idx)
                     (let ((name (binding-name refvar attr)))
                       (or (gethash name *bindings*) ;;; or returns the first non-nil argument and skips evaluating the rest of its args.
                           (let ((new (make-binding :ref refvar :attr attr :map map)))
                             (setf (b-unwatch new) (define-watch refvar attr new))
                             (setf (gethash name *bindings*) new)))))
            finally (return binding-array))))
  (:documentation "bind a ref (or an array of refs) to an attr of a html element. This
will establish a watch function, which will automatically set the attr
of all registered html elements on state change of the
refvar. Registering html elements is done by pushing the html element
to the b-elist slot of the binding (normally done in the creation
function of the html element). The method returns the binding or an
array of bindings, depending on the class."))

 ;;; (setf (gethash...) ) returns the value which got set (new in this case).

(defun bind-refs-to-attrs (&rest refs-and-attrs)
  (loop
    for (ref attr) on refs-and-attrs by #'cddr
    collect (cond
              ((arrayp ref) (loop for r across ref collect (bind-ref-to-attr r attr)))
              (t (bind-ref-to-attr ref attr)))))

(defun obj-print (seq)
  (format nil "(~{~a~^ ~})"
          (mapcar (lambda (x)
                    (cond
                      ((functionp x) (format nil "<function {~a}>" (sb-kernel:get-lisp-obj-address x)))
                      (:else (format nil "~a" x))))
                  seq)))

(defmacro b-unregister (element binding)
  "Unregister element from binding e-list, removing the binding from the
hash-table if its e-list is empty."
  `(progn
     (setf (b-elist ,binding) (remove ,element (b-elist ,binding)))
     (unless (b-elist ,binding)
       (remhash (binding-name (b-ref ,binding) (b-attr ,binding))
                *bindings*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       clog part                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; little clog extension to allow simple data transport js -> lisp

(defparameter *data-event-script*
  "+ JSON.stringify(data)")

(defun parse-data-event (data)
  (yason:parse data))

(defmethod set-on-data ((obj clog-obj) handler)
   (clog::set-event
    obj "data"
    (when handler
      (lambda (data)
        (unless (string= data "undefined")
          (funcall handler obj (parse-data-event data)))))
    :call-back-script *data-event-script*))

#|
(defun format-style (css)
  (if css (format nil "style=\"~@[~{~(~A~): ~(~a~);~}~]\"" css)))
|#

(defun format-style (css)
  (if css (format nil "style=\"~{~a: ~a~^; ~}\"" css)))

;;; (format-style '(:width 1em :height 2em))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; creation functions for gui widgets
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; o-knob is a custom html element defined in js:

(defun create-o-knob (parent bindings &key (unit "") (precision 2) min max width height step css)
  (declare (ignorable width height))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings)))
         (element (create-child
                   parent
                   (format nil "<o-knob min=\"~a\" max=\"~a\" step=\"~a\" value=\"~a\" precision=\"~a\" unit=\"~a\" ~@[~a~]></o-knob>"
                           min max step (get-val var) precision unit
                           (format-style css))))) ;;; the get-val automagically registers the ref
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                     ;;                     (or (if (gethash "close" data) "close")
                     ;;                         (gethash attr data))))
                     (if (gethash "close" data)
                         (progn
;;;                           (format t "closing knob~%")
                           (dolist (binding bindings) (setf (b-elist binding) (remove element (b-elist binding))))) ;;; cleanup: unregister elem.
                         (progn
                           (%set-val var (float (gethash attr data) 1.0)))
                         ))))
    element))

(defun create-o-numbox (parent bindings &key min max width height (precision 2) css)
  (declare (ignorable width height))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings)))
         (element (create-child
                   parent
                   (format nil "<o-numbox min=\"~a\" max=\"~a\" value=\"~a\" precision=\"~a\" ~@[~a~]>"
                           min max (get-val var) precision
                           (format-style css))))
         ) ;;; the get-val automagically registers the ref
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data ;;; react to changes in the browser page
     element
     (lambda (obj data)
       (declare (ignore obj))
       ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
       ;;                     (or (if (gethash "close" data) "close")
       ;;                         (gethash attr data))))
       (if (gethash "close" data)
           (progn
;;;             (format t "closing numbox~%")
             (dolist (binding bindings) (setf (b-elist binding) (remove element (b-elist binding))))) ;;; cleanup: unregister elem.
           (let ((*refs-seen* (list (list element attr))))
;;;             (format t "~&numbox recv value: ~a, ~a~%" (float (gethash attr data) 1.0) *refs-seen*)
             (%set-val var (float (gethash attr data) 1.0))
             ))))
    element))

(defun array->js-string (array)
  (with-output-to-string (str)
    (format str "[~a" (aref array 0))
    (map '() (lambda (x) (format str ", ~a" x))
         (make-array (1- (length array))
                     :displaced-to array
                     :displaced-index-offset 1))
    (format str "]")))

(defun buffer->js-string (buffer)
  (with-output-to-string (str)
    (if (> (incudine:buffer-size buffer) 0)
        (progn
          (format str "[~,2f" (incudine:buffer-value buffer 0))
          (dotimes (i (1- (incudine:buffer-size buffer)))
            (format str ", ~,2f" (incudine:buffer-value buffer (1+ i))))
          (format str "]")))
    "[0]"))

(defun create-o-scope (parent bindings &key width height css buffer)
  "Create a scope view using /buffer/ as data for the scope. buffer has
to be a ref-object containing an incudine buffer updating the view
whenever the ref gets set with set-val."
  (declare (ignorable width height))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings)))
         (element (create-child
                   parent
                   (format nil "<o-scope ~@[~a~]></o-scope>"
                           (format-style css))))
         (unwatch (watch (lambda ()
                           (execute element (format nil "setValues(~a)"
                                                    (buffer->js-string (get-val buffer)))))))) ;;; the get-val automagically registers the ref
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                     ;;                     (or (if (gethash "close" data) "close")
                     ;;                         (gethash attr data))))
                     (if (gethash "close" data)
                         (progn
;;;                           (format t "closing scope~%")
                           (dolist (binding bindings) (setf (b-elist binding) (remove element (b-elist binding)))) ;;; cleanup: unregister elem.
                           (funcall unwatch)
                           )
                         (progn
                           (%set-val var (float (gethash attr data) 1.0)))
                         ))))
    element))

;;; (setf *refs-seen* nil)

(defmacro option-main (option)
  `(if (listp ,option)
       (first ,option)
       ,option))

(defmacro option-second (option)
  `(if (listp ,option)
       (or (second ,option) (first ,option))
       ,option))

(defun opt-format-attr (attr val)
  (format nil "~a='~(~a~)'" attr (or val 'false) ))


(defun clog-trigger-fn (obj)
  "execute bang() on the clog obj."
  (lambda () (clog:js-execute obj (format nil "~a.bang()" (clog:script-id obj)))))

;;; (funcall (clog-trigger-fn (elt *bangs* 0)))


(defun create-o-bang (parent bindings &key width height label (background '("transparent" "orange")) color flash-time css (flash t))
  (declare (ignorable width height))
  (let* ((var (b-ref (first bindings)))
;;;         (attr (b-attr (first bindings)))
         (element (create-child
                   parent
                   (format nil "<o-bang ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-bang>"
                           (list
                            (opt-format-attr "label-off" (option-main label))
                            (opt-format-attr "label-on" (option-second label))
                            (opt-format-attr "background-off" (option-main background))
                            (opt-format-attr "background-on" (option-second background))
                            (opt-format-attr "color-off" (option-main color))
                            (opt-format-attr "color-on" (option-second color))
                            (opt-format-attr "flash-time" flash-time)
                            (opt-format-attr "flash" (if flash 1 0)))
                           (format-style css)
                           (or (option-main label) "")))))
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (incudine.util:msg :info "clog event from ~a: ~{~{~S~^ ~}~^,~}" (html-id element)
                                      (ou:hash-table-contents data))
                   (cond ((gethash "close" data)
                          (unregister-element element bindings)) ;;; cleanup: unregister elem.
                         (t (let ((*refs-seen* (list (list obj "bang"))))
                              ;; (incudine.util:msg :info "~&triggering: ~a~%" var
                              ;;                    (or (if (gethash "close" data) "close")
                              ;;                        (gethash attr data)))
                              
                              (%trigger var))))))
    element))

(defun unregister-element (element bindings)
  "Remove element from all bindings, removing the binding from the
hash-table if its element-list is empty."
  (dolist (binding bindings)
    (b-unregister element binding)))

(defun array->attr (arr)
  (format nil "[~{~a~^, ~}]" (coerce arr 'list)))

(defun create-o-toggle (parent bindings &key width height label (background '("transparent" "orange")) color flash-time values css)
  (declare (ignorable width height))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings)))
         (element (create-child
                   parent
                   (format nil "<o-toggle ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-toggle>"
                           (list
                            (opt-format-attr "value" (get-val var))
                            (opt-format-attr "label-off" (option-main label))
                            (opt-format-attr "label-on" (option-second label))
                            (opt-format-attr "background-off" (option-main background))
                            (opt-format-attr "background-on" (option-second background))
                            (opt-format-attr "color-off" (option-main color))
                            (opt-format-attr "color-on" (option-second color))
                            (opt-format-attr "flash-time" flash-time)
                            (opt-format-attr "value-off" (or (first values) 0))
                            (opt-format-attr "value-on" (or (second values) 1)))
                           (format-style css)
                           (or (option-main label) "")))))
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list (list element attr))))
                     ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                     ;;                     (or (if (gethash "close" data) "close")
                     ;;                         (gethash attr data))))
                     (cond ((gethash "close" data)
                            (unregister-element element bindings))
                           (t (%set-val var (read-from-string (gethash attr data))))
                           ))))
    element))

(defun create-o-radio (parent bindings &key labels label width height (background '(("transparent") ("orange")))
                                        color flash-time values (num 8) (direction :right) css)
  (declare (type (member :up :right :down :left) direction)
           (ignorable width height))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings))) ;;; format nil "~{~a~^,~}"
         (element (create-child
                   parent
                   (format nil "<o-radio ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-radio>"
                           (list
                            (opt-format-attr "value" (round (get-val var)))
                            (opt-format-attr "label-off" (if (option-main labels) (format nil "~{~a~^,~}" (option-main labels))))
                            (opt-format-attr "label-on" (if (option-second labels) (format nil "~{~a~^,~}" (option-second labels))))
                            (opt-format-attr "background-off" (if (option-main background) (format nil "~{~a~^,~}" (option-main background))))
                            (opt-format-attr "background-on" (if (option-second background) (format nil "~{~a~^,~}" (option-second background))))
                            (opt-format-attr "color-off" (if (option-main color) (format nil "~{~a~^,~}" (option-main color))))
                            (opt-format-attr "color-on" (if (option-second color) (format nil "~{~a~^,~}" (option-second color))))
                            (opt-format-attr "flash-time" flash-time)
                            (opt-format-attr "value-off" (or (first values) 0))
                            (opt-format-attr "value-on" (or (second values) 1))
                            (opt-format-attr "data-num" (or num 8))
                            (opt-format-attr "direction" direction))
                           (format-style css)
                           (or (option-main label) "")))))
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list (list element attr))))
                     (cond ((gethash "close" data)
                            (unregister-element element bindings))
                           (t (%set-val var (gethash attr data)))))))
    element))

(defun create-o-slider (parent bindings &key (direction :up) (min 0) (max 1)
                                         label background thumb-color bar-color
                                         (mapping :lin) (clip-zero nil)
                                         (width "1em") (height "8em") padding css)
  (declare (type (member :lin :log) mapping)
           (type (member :up :right :down :left) direction))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings))) ;;; format nil "~{~a~^,~}"
         (element (create-child
                   parent
                   (format nil "<o-slider ~{~@[~a ~]~}>~@[~a~]</o-slider>"
                           (list
                            (format-style (append `(:width ,width :height ,height :padding ,padding) css))
                            (opt-format-attr "direction" direction)
                            (opt-format-attr "value" (float (get-val var) 1.0))
                            (opt-format-attr "min" min)
                            (opt-format-attr "max" max)
                            (opt-format-attr "label" label)
                            (opt-format-attr "background" (or background "white"))
                            (opt-format-attr "thumb-color" (or thumb-color "black"))
                            (opt-format-attr "bar-color" (or bar-color "transparent") )
                            (opt-format-attr "mapping" mapping )
                            (opt-format-attr "clip-zero" clip-zero ))
                           (or (option-main label) "")))))
    (dolist (binding bindings)
      (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding)))))
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list (list element attr)))) ;;; set context for %set-val below
                     (cond ((gethash "close" data)
                            (unregister-element element bindings))
                           (t (%set-val var (gethash attr data)))))))
    element))

;;;   <li>Data Interval: <span id=\"Accelerometer_i\">0</span><span> ms</span></li>

(defun sensor-data-reader-fn (&rest data)
  data)

(defun create-o-sensor (parent bindings &key (interval 2) (orientation t) (xyz t) (gxyz t) (gyro t) css)
  (let* (;;; (var (b-ref (first bindings)))
         ;;; (attr (b-attr (first bindings))) ;;; format nil "~{~a~^,~}"
         (element (create-child
                   parent
                   (format nil "<o-sensor ~{~@[~a ~]~}>
<div id=\"trigger\" style=\"background: transparent; width: 100%; height: 3em;border: 1px solid black;\"></div>
<h4>Delta G</h4>
<ul>
  <li>delta-g: <span id=\"Delta_g\">0</span></li>
  <li>Trigger-timeout: <span id=\"Trigger_timeout\">0</span><span> ms</span></li>
  <li>Trigger-threshold: <span id=\"Trigger_threshold\">0</span></li>
  <li>Trigger-active: <span id=\"Trigger_active\">0</span></li>
</ul>
<div id=\"delta_g_panel\" style=\"display: inline-block;height: 10rem;\"\>
<div id=\"orientation_panel\" style=\"display: inline-block;height: 10rem;\"\>
<h4 style=\"margin-top:2.75rem;\">Orientation</h4>
<ul>
  <li>X-axis (β): <span id=\"Orientation_b\">0</span><span>°</span></li>
  <li>Y-axis (γ): <span id=\"Orientation_g\">0</span><span>°</span></li>
  <li>Z-axis (α): <span id=\"Orientation_a\">0</span><span>°</span></li>
</ul>
</div>

<div id=\"accelerometer_panel\" style=\"display: inline-block;height: 10rem;\"\>
<h4>Accelerometer</h4>
<ul>
  <li>X-axis: <span id=\"Accelerometer_x\">0</span><span> m/s<sup>2</sup></span></li>
  <li>Y-axis: <span id=\"Accelerometer_y\">0</span><span> m/s<sup>2</sup></span></li>
  <li>Z-axis: <span id=\"Accelerometer_z\">0</span><span> m/s<sup>2</sup></span></li>
</ul>
</div>

<div id=\"g_accelerometer_panel\" style=\"display: inline-block;height: 10rem;\"\>
<h4>Accelerometer including gravity</h4>

<ul>
  <li>X-axis: <span id=\"Accelerometer_gx\">0</span><span> m/s<sup>2</sup></span></li>
  <li>Y-axis: <span id=\"Accelerometer_gy\">0</span><span> m/s<sup>2</sup></span></li>
  <li>Z-axis: <span id=\"Accelerometer_gz\">0</span><span> m/s<sup>2</sup></span></li>
</ul>
</div>

<div id=\"gyroscope_panel\" style=\"display: inline-block;height: 10rem;\"\>
<h4>Gyroscope</h4>
<ul>
  <li>X-axis: <span id=\"Gyroscope_x\">0</span><span>°/s</span></li>
  <li>Y-axis: <span id=\"Gyroscope_y\">0</span><span>°/s</span></li>
  <li>Z-axis: <span id=\"Gyroscope_z\">0</span><span>°/s</span></li>
</ul>
</div>
</o-sensors>"
                           (list
                            (format-style css)
                            (opt-format-attr "interval" interval)
                            (opt-format-attr "id" nil)
                            (opt-format-attr "orientation" orientation)
                            (opt-format-attr "xyz" xyz)
                            (opt-format-attr "gxyz" gxyz)
                            (opt-format-attr "gyro" gyro)
                            )))))
    (dolist (binding bindings)
      (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding)))))
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (setf *test* data)
;;;                   (break "data: ~a" data)
                   (cond ((gethash "close" data)
                          (unregister-element element bindings))
                         (t (dolist (binding bindings)
                              (let* ((attr (b-attr binding))
                                     (*refs-seen* (list (list element attr)))
                                     (val (gethash attr data)))
                                (when val
                                  (cond
                                    ((string= attr "sensor-data")
                                     (push (list element attr) *refs-seen*)
                                     (%set-val (b-ref binding)
                                               (apply #'sensor-data-reader-fn (read-from-string val))))
                                    ((string= attr "sensor-trigger")
                                     (push (list element attr) *refs-seen*)
                                     (%set-val (b-ref binding) val)
                                     (if (= val 1) (trigger (b-ref binding))))
                                    (t
                                     (push (list element attr) *refs-seen*)
                                     (%set-val (b-ref binding) val))))))))))
    element))

(defparameter *test* nil)
;;; min, max, mapping, clip-zero, thumb-color, bar-color

(defun create-o-multislider (parent bindings
                             &key (direction :up) (value 0) (min 0) (max 1)
                             width height
                               label background colors (thumb-color "transparent")
                               (mapping :lin) (clip-zero nil))
  (declare (ignorable width height)
           (type (member :lin :log) mapping)
           (type (member :up :right :down :left) direction))
  (let* ((num-sliders (length (first bindings)))
         (element (create-child
                      parent
                      (format nil "<o-multislider ~{~@[~a ~]~}>~@[~a~]</o-multislider>"
                              (list
                               (opt-format-attr "num-sliders" num-sliders)
                               (opt-format-attr "direction" direction)
                               (opt-format-attr "value" value)
                               (opt-format-attr "min" min)
                               (opt-format-attr "max" max)
                               (opt-format-attr "label" label)
                               (opt-format-attr "background" (or background "white"))
                               (opt-format-attr "colors" (if colors (format nil "~{~a~^,~}" colors)))
                               (opt-format-attr "mapping" mapping )
                               (opt-format-attr "clip-zero" clip-zero ))
                              (or (option-main label) "")))))
    (loop for binding in (first bindings)
          collect (create-o-slider element (list binding)
                                   :thumb-color (or thumb-color "transparent")
                                   :direction direction))
    (execute element (format nil "initSliders(~a)" num-sliders))
    element))

(defun create-o-vumeter (parent bindings &key (direction :up)
                                (type :led) (mapping :db-lin)
                                          (width "1em") (height "8em") padding css)
  (declare (type (member :up :right :down :left) direction)
           (type (member :led :bar) type)
           (type (member :pd :db-lin) mapping))
  (let* ((var (b-ref (first bindings)))
         (attr (b-attr (first bindings))) ;;; format nil "~{~a~^,~}"
         (element (create-child
                   parent
                   (format nil "<o-vumeter ~{~@[~a ~]~}></o-slider>"
                           (list
                            (format-style (append `(:width ,width :height ,height :padding ,padding) css))
                            (opt-format-attr "led-mapping" mapping)
                            (opt-format-attr "direction" direction)
                            (opt-format-attr "db-value" (float (get-val var) 1.0))

                            (opt-format-attr "type" type))))))
    (dolist (binding bindings) (push element (b-elist binding))
            (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding))))) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list (list element attr)))) ;;; set context for %set-val below
                     ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                     ;;                     (or (if (gethash "close" data) "close")
                     ;;                         (gethash attr data))))
                     (cond ((gethash "close" data)
                            (unregister-element element bindings))))))
    element))

(defun create-o-svg (parent bindings &key svg padding css (cursor-pos 0) (shift-x 0) (shift-y 0) (background "#fff") (scale 1) (inverse 0))
  (let ((element (create-child
                   parent
                   (format nil "<o-svg ~{~@[~a ~]~}></object>"
                           (list
                            (opt-format-attr "data" svg)
                            (opt-format-attr "cursor-pos" cursor-pos)
                            (opt-format-attr "shift-x" shift-x)
                            (opt-format-attr "shift-y" shift-y)
                            (opt-format-attr "scale" scale)
                            (opt-format-attr "inverse" inverse)
                            (opt-format-attr "data" svg)
                            (format-style (append
                                           `(:padding ,padding
                                             :background ,background)
                                           css)))))))
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-attribute-form (get-val (b-ref binding)))))
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignorable obj))
                   ;; (incudine.util:msg :info "~&~%clog event from ~a: ~a~%" element
                   ;;                    (or (if (gethash "close" data) "close")
                   ;;                        (gethash attr data)))
                   (cond
                     ((gethash "close" data)
                      (unregister-element element bindings))
                     (t
                      (dolist (binding bindings)
                        (let* ((attr (b-attr binding))
                               (*refs-seen* (list (list element attr)))
                               (val (gethash attr data)))
                          (when val
                            ;; (if (listp val)
                            ;;     (format t "attr: ~a, val: ~{~,2f~^, ~}, refs-seen: ~a~%" (b-attr binding) val *refs-seen*)
                            ;;     (format t "attr: ~a, val: ~a, refs-seen: ~a~%" (b-attr binding) val *refs-seen*))
                            (push (list element attr) *refs-seen*)
                            (%set-val (b-ref binding) val)))

                        )) ;;; cleanup: unregister elem.
                     )))
    element))

(defmacro expand-bindings (bindings)
  `(loop for binding in ,bindings
         collect `((gethash ,(b-attr binding))
                   (%set-val ,(b-ref binding) (gethash ,(b-attr binding) data)))))

(defun create-hide-button (parent element-to-hide
                           &key label (background '("transparent" "orange"))
                             color flash-time values css (val 1) auto-place)
  (let* ((button (create-child
                   parent
                   (format nil "<o-toggle ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-toggle>"
                           (list
                            (opt-format-attr "value" val)
                            (opt-format-attr "label-off" (option-main label))
                            (opt-format-attr "label-on" (option-second label))
                            (opt-format-attr "background-off" (option-main background))
                            (opt-format-attr "background-on" (option-second background))
                            (opt-format-attr "color-off" (option-main color))
                            (opt-format-attr "color-on" (option-second color))
                            (opt-format-attr "flash-time" flash-time)
                            (opt-format-attr "value-off" (or (first values) 0))
                            (opt-format-attr "value-on" (or (second values) 1)))
                           (format-style css)
                           (or (option-main label) ""))
                   :auto-place auto-place)))
    (set-on-data button ;;; react to changes in the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (let ((*refs-seen* (list button)))
                     ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" button
                     ;;                     (or (if (gethash "close" data) "close")
                     ;;                         (gethash "value" data))))
                     (cond ((gethash "close" data)
                            (progn
;;;                              (format t "closing hide button~%")
                              ))
                           (t (setf (hiddenp element-to-hide)
                                    (zerop (read-from-string
                                            (gethash "value" data)))))))))
    button))

;; array as attribute: <div id="demo" data-stuff='["some", "string", "here"]'></div>
;;
;; <div id="storageElement" data-storeIt="stuff,more stuff"></div> and use string.split.
;;
;; this is just how I would structure such a dynamic website. With a
;; 12 column layout with collection of input elements

(defun create-collection (parent width)
  (create-child parent (format nil "<div data-width='~a' class='collection'></div>" width)))

(defun create-grid (parent class width)
  (create-child parent (format nil "<div data-width='~a' class='~a'></div>" width class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; utils from clog-dsp-widgets (will be replaced soon)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric flash (clog-obj)
  (:method ((obj clog-obj))
    (execute obj "bang()"))
  (:documentation "Call the bang() function of /clog-obj/ without triggering its bang
event.

@Arguments
clog-obj - A Clog object which accepts a bang() function, like a Button.
"))

(defgeneric pulse-on (clog-obj &optional freq)
  (:method ((obj clog-obj) &optional (freq 2))
    (execute obj (format nil "pulseOn(~A)" (round (/ 1000 freq 2.0)))))
  (:documentation "Call the pulseOn() function of clog-obj. This results in a flashing of
/clog-obj/ alternating between foreground and background color with
frequency /freq/.

@Arguments
clog-obj - A Clog object which accepts a pulseOn() function, like a Button or a Toggle.
freq - Number denoting the flash frequency.
"))

(defgeneric pulse-off (clog-obj)
  (:method ((obj clog-obj))
    (execute obj "pulseOff()"))
  (:documentation "Call the pulseOff() function of /clog-obj/, restoring its background
color.

@Arguments
clog-obj - A Clog object which accepts a pulseOff() function, like a Button or a Toggle.
"))

(defgeneric highlight (clog-element value)
  (:documentation "Highlight /clog-element/ depending on /value/ (0 unhighlights, all
other values highlight).

@Arguments
clog-obj - A Clog object which accepts a highlight() function, like a Button or a Toggle.
"))

(defmethod highlight ((obj clog-element) value)
  (execute obj (format nil "highlight(~A)" value))  
  value)

;;; We don't want to restart the server everytime when the new-window
;;; fun is canged thats why this proxy gets defined
(defun on-new-window (body)
  (new-base-gui-window body))

;; Initialize the CLOG system with a boot file which contains the
;; static js files. For customized uses copy the "www" subdirectory of
;; the repository to your local project and adjust :static-root
;; accordingly

(defun new-base-gui-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Clog Gui")
  (add-class body "w3-blue-grey"))

(defun start-gui (&key (port 54619) (gui-base (asdf:system-source-directory :clog-dsp-widgets)) (open t) ssl ssl-cert-file ssl-key-file)
  (clear-bindings) ;;; start from scratch
  (format t "starting webserver at ~A" (merge-pathnames "www/" gui-base))
  (initialize #'on-new-window
              :port port
              :static-root (merge-pathnames "www/" gui-base)
              :ssl ssl
              :ssl-key-file ssl-key-file
              :ssl-cert-file ssl-cert-file
              :boot-file "/boot.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (when open (open-browser)))

;;; (start-gui) should start a webserver

;;; (merge-pathnames "www/" (pathname "/tmp/"))
