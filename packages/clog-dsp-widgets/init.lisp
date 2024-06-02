;;; 
;;; init.lisp
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

(in-package :clog-dsp-widgets)

(defun clog-dsp-widgets-initialize (body)
  (clog-gui-initialize body)
  (load-script (html-document body) "js/components.js")
  (load-script (html-document body) "js/bang.js")
;;;  (load-script (html-document body) "js/components.js")
  (load-script (html-document body) "js/vumeter.js")
  (load-script (html-document body) "js/toggle.js")
  (load-script (html-document body) "js/slider.js")
  (load-css (html-document body) "/css/w3.css")
  (load-css (html-document body) "./css/custom-gui-elems.css"))
