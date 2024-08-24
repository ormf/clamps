;;; svg-macros.lisp
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************


(in-package :svg-import-export)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar +black+ "#000000")
  (defvar +white+ "#FFFFFF")

  (defmacro def-exporting-class (name (&rest superclasses) (&rest slot-specs)
                                 &optional class-option)
    (let ((exports (mapcan (lambda (spec)
                             (when (getf (cdr spec) :export)
                               (let ((name (or (getf (cdr spec) :accessor)
                                               (getf (cdr spec) :reader)
                                               (getf (cdr spec) :writer))))
                                 (when name (list name)))))
                           slot-specs)))
      `(progn
         (defclass ,name (,@superclasses)
           ,(append 
             (mapcar (lambda (spec)
                       (let ((export-pos (position :export spec)))
                         (if export-pos
                             (append (subseq spec 0 export-pos)
                                     (subseq spec (+ 2 export-pos)))
                             spec)))
                     slot-specs)
             (when class-option (list class-option))))
         ,@(mapcar (lambda (name) `(export ',name))
                   exports))))

  (defmacro sv (obj slot)
    `(slot-value ,obj ,slot))

  (defmacro format-with-slots (stream obj string &rest slots)
    `(apply #'format 
            (append (list ,stream ,string) 
                    (loop for x in ',slots collect (funcall x ,obj))))))
