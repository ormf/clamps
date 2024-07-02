;;; 
;;; inkscape-export.lisp
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

(in-package :cm)

(defparameter *osc-inkscape-export-in* nil)
;;; (defparameter cl-user::*tmpsnd* nil)

#|
(defun port-available-p (portno)
  (string= ""
           (string-trim '(#\NEWLINE)
                        (with-output-to-string (out)
                          (uiop::run-program (format nil "lsof -i:~d" portno)
                                             :ignore-error-status t
                                             :output out)))))
|#






 ;;; (stop-inkscape-osc)
;;; (start-inkscape-osc)

(export '(*osc-inkscape-export-in* fn-start-inkscape-osc fn-stop-inkscape-osc start-inkscape-osc stop-inkscape-osc) :cm)

