;;;
;;; clog-cuda.asd
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
;;; extension to clog-dsp-widgets for incudine
;;;


(asdf:defsystem #:clog-cuda
  :description "clog widgets for use with incudine"
  :depends-on (:yason :clog :of-incudine-dsps :clog-dsp-widgets :cl-refs)
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "incudine-setup")
               (:file "dsp-registry")
               (:file "gui-base-class")
;;;               (:file "levelmeter-incudine")
               (:file "utils")
               (:file "bus")
               (:file "levelmeter-gui")
               (:file "scope-incudine")
               (:file "scope-gui")
               (:file "clog-cuda")))
