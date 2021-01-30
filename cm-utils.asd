;;; cm-utils.asd
;;;
;;; Copyright (c) 2017 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


(asdf:defsystem #:cm-utils
  :description "Utilities for Common Music 2"
  :author "Orm Finnendahl <orm.finnendahl@selma-hfmdk-frankfurt.de>"
  :license "LLGPL"
  :serial t
  :depends-on (#:uiop
               #:incudine
               #:orm-utils
               #:fomus
               #:cm-fomus
               #:cm-svg
               #:cl-ppcre
               #:cl-coroutine
               #:cm-incudine
               #:cl-plot
               #:incudine-plot)
  :components ((:file "package")
               (:file "cm-utils")
               (:file "patterns")))

