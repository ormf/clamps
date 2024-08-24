;;; svg-export.lisp
;;;
;;; main export-svg-file routine and helpers
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************


(in-package #:svg-import-export)

;;; "svg-import-export" goes here. Hacks and glory await!

(setf *print-case* :downcase)

(defparameter *inverse* nil)

(defun get-color (color)
  (if (and *inverse* (string/= color "none"))
      (format nil "#~6,'0X" (- #XFFFFFF (read-from-string (format nil "#X~a" (string-left-trim '(#\#) color)))))
      color))

(defun new-id (svg-file id-type)
  (incf (gethash id-type (slot-value svg-file 'id-hash) 0)))

(defgeneric add-elements (container-obj &rest elems)
  (:documentation "add elements supplied either as svg-objects or as list(s) of svg-objects"))

(defmethod add-elements ((svg svg-file) &rest elems)
  "add elements supplied either as svg-objects or as list(s) of svg-objects"
  (dolist (elem (reverse elems))
    (if (consp elem)
        (dolist (element elem)
          (push element (elements svg)))
        (push elem (elements svg)))))

(defmethod add-elements ((obj svg-group) &rest elems)
  "add elements supplied either as svg-objects or as list(s) of svg-objects"
  (list (cons obj
              (let (result)
                (dolist (elem (reverse elems))
                  (if (consp elem)
                      (dolist (element elem)
                        (push element result))
                      (push elem result)))
                result))))

(defun print-elements-to-stream (elems stream)
  (cond ((null elems) nil)
        ((consp (car elems))
         (progn
           (print-head-to-stream (caar elems) stream)
           (print-elements-to-stream (cdar elems) stream)
           (print-tail-to-stream (caar elems) stream)
           (print-elements-to-stream (cdr elems) stream)))
        (t (progn
           (print-to-stream (car elems) stream)
           (print-elements-to-stream (cdr elems) stream)))))

(defun export-svg-file (svg-file &key (fname "/tmp/test.svg" fname-supplied-p) (inverse nil) (showgrid t) (gridtype "4x4") width
                                   (zoom 1.4) (cx 350) (cy 360) (w-width 1920) (w-height 1080) (w-x 0) (w-y 0))
  (setf (sv svg-file 'inverse) inverse)
  (setf (sv svg-file 'showgrid) showgrid)
  (setf (sv svg-file 'gridtype) gridtype)
  (setf (sv svg-file 'width) (or width (sv svg-file 'width) 10000))
  (setf (sv svg-file 'zoom) zoom)
  (setf (sv svg-file 'cx) cx)
  (setf (sv svg-file 'cy) cy)
  (setf (sv svg-file 'w-width) w-width)
  (setf (sv svg-file 'w-height) w-height)
  (setf (sv svg-file 'w-x) w-x)
  (setf (sv svg-file 'w-y) w-y)
  (setf (sv svg-file 'fname) (if fname-supplied-p fname (or (sv svg-file 'fname) fname)))
  (with-open-file (outstream (sv svg-file 'fname) :direction :output :if-exists :supersede)
    (print-head-to-stream svg-file outstream)
    (print-elements-to-stream (elements svg-file) outstream)
    (print-tail-to-stream svg-file outstream))
  (sv svg-file 'fname))

(defun export-svg (svg-file)
  (with-slots (fname) svg-file
    (with-open-file (outstream fname :direction :output :if-exists :supersede)
      (print-head-to-stream svg-file outstream)
      (print-elements-to-stream (elements svg-file) outstream)
      (print-tail-to-stream svg-file outstream))
    fname))

(defmacro with-svg-file ((svg-file &rest keys) &body body)
  `(let ((,svg-file (apply #'make-instance 'svg-file ',keys)))
     ,@body))
