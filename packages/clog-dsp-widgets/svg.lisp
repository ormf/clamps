;;; 
;;; svg.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :clog-dsp-widgets)

(defun create-o-svg (parent svg &key padding css (translate 0))
  (let* ((element (create-child
                   parent
                   (format nil "<object ~{~@[~a ~]~}></object>"
                           (list
                            (format-style (append
                                           `(:padding ,padding
                                             :transform ,(format nil "translate(~apx)" translate))
                                           css))
                            (opt-format-attr "id" "svg")
                            (opt-format-attr "data" svg)
                            "type=\"image/svg+xml\"")))))
    element))
