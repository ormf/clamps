;;; 
;;; clog-redefs.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :clog)

(setf *print-case* :downcase)
(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

;;; (make-keyword 'name)  ; => :name
;;; (make-keyword "NaMe")  ; => :name

(defun move-to-css (props css)
  (loop for prop in props
    collect `(when ,prop (setf (getf ,css ,(make-keyword prop)) ,prop))))

;;;       ,@(move-to-css '(visibility) 'css)

(defun html-tag-format-string (tag &optional content)
  "create a html format string for the create-... functions for a given
tag name. If content is t provide a slot for the content of the html
tag."
  (if content
      (format nil "<~A ~~{~~(~~A~~)= \"~~(~~a~~)\"~~^ ~~}>~~A</~A>" tag tag)
      (format nil "<~A ~~{~~(~~A~~)= \"~~(~~a~~)\"~~^ ~~}/>" tag)))

(defmacro args->attribute-plist (args &key remove)
  "transform args that it only contains a property list of attributes to
be used for a html tag. The string for the html style attribute is
also constructed here. It can be provided either directly as a string
in the style arg of this macro or as a property list within args using
the :css keyword, e.g.

 :css '(:position \"relative\" :width 120 :display \"flex\" ...)

The :remove key allows to specify additional properties to be removed
from args which are not intended for the html (see the #'create-label
method for an example).

Example:

(args->attribute-plist '(:val 100 :css (:position \"absolute\" :width 200) :html-id \"myID\" :auto-place nil :myprop \"unused\")
                           \"myClass\"
                           t
                           \"height: 120;\"
                           :remove (:myprop)

 => (:style \"visibility: hidden;position: absolute;width: 200;height: 120;\" :val 100 :class \"myClass\")

  "
  (let ((my-args (gensym "args")))
    `(let* ((,my-args ,args)
            (class (getf ,my-args :class))
            (css (getf ,my-args :css))
            (style (getf ,my-args :style)))
       (when (getf ,my-args :hidden) (setf (getf css :visibility) "hidden"))
       (when class (setf (getf ,my-args :class)
                         (format nil "~A" (escape-string class :html t))))
       (when (or style css)
         (setf (getf ,my-args :style)
               (format nil "~@[~{~(~A~): ~(~a~);~}~]~@[~a~]" css style)))
       (setf (getf args :draggable) (or (getf args :draggable) "false"))
       (dolist (key (append '(:css :hidden :html-id :auto-place) ,remove)) (remf ,my-args key))
       ,my-args)))

(defun attr-sanitize (args)
  (loop
    for (key val) on args by #'cddr
    append (list key (if (and (vectorp val) (char= (aref val 0) #\')) val (format nil "\"~a\"" val)))))

(defun format-html-tag (tag args &optional content)
  "create a html format string for the create-... functions for a given
tag name. To enforce a closing tag without innerHtml, supply an empty
string as content argument."
;;;  (break "<~A ~{~(~A~)= ~(~a~)~^ ~}>~A</~A>" tag (attr-sanitize args) content tag)
  (if content
      (format nil "<~A ~{~(~A~)= ~(~a~)~^ ~}>~A</~A>" tag (attr-sanitize args) content tag)
      (format nil "<~A ~{~(~A~)= ~(~a~)~^ ~}/>" tag (attr-sanitize args))))

(defmethod create-div ((obj clog:clog-obj) &rest args
                            &key content class style hidden html-id (auto-place t)
                            &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag :div
                                     (args->attribute-plist args)
                                     (or content ""))
                :clog-type  'clog-div
                :html-id    html-id
                :auto-place auto-place))

(defmethod create-form-element ((obj clog-obj) element-type
                                &rest args
                                &key name label class style hidden html-id (auto-place t)
                                &allow-other-keys)
  (declare (ignorable name class style hidden))
  (setf (getf args :type) (escape-string element-type :html t))
  (let* ((element (create-child
                   obj (format-html-tag
                        :input
                        (args->attribute-plist args))
                   :clog-type  'clog-form-element
                   :html-id    html-id
                   :auto-place auto-place)))
    (when label
      (label-for label element))
    element))

(defmethod create-button ((obj clog-obj)
                          &rest args
                          &key content class style hidden html-id (auto-place t)
                          &allow-other-keys)
  (declare (ignorable class style hidden))
  (create-child obj (format-html-tag
                     :button
                     (args->attribute-plist args)
                     content)
                :clog-type  'clog-button
                :html-id    html-id
                :auto-place auto-place))

(defmethod create-label ((obj clog-obj)
                         &rest args
                         &key content label-for class style hidden html-id (auto-place t)
                         &allow-other-keys)
  (declare (ignorable class style hidden))
  (when label-for (setf (getf args :for) (html-id label-for)))
  (create-child obj (format-html-tag
                     :label
                     (args->attribute-plist args :remove '(:label-for))
                     (or content ""))
                :clog-type  'clog-label
                :html-id    html-id
                :auto-place auto-place))


(defmethod value ((obj clog-form-element))
  (property obj "value"))

(defmethod (setf text-value) (value (obj clog-form-element))
  (setf (property obj "value") value))
