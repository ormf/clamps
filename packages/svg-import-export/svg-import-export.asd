;;; svg-import-export.asd
;;;
;;; **********************************************************************
;;; Copyright (C) 2012-2018 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

(asdf:defsystem #:svg-import-export
  :serial t
  :depends-on (#:xpath
               #:cxml-stp
               #:cl-ppcre
               #:cl-FAD
               #:orm-utils)
  :components ((:file "package")
               (:file "svg-macros")
               (:file "svg-classes" :depends-on ("svg-macros"))
               (:file "svg-export" :depends-on ("svg-classes"))
               (:file "svg-import" :depends-on ("svg-classes"))
	       (:file "svg-import-export-utils")))

