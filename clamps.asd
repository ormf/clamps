;;; clamps.asd

(in-package :cl-user)

;;; avoid xlock violation error on xpath:

(ql:quickload :xpath)
(ql:quickload :cxml-stp)

;;; make sure svg-import-export and clog-dsp-widgets are loaded from
;;; the packages subdir rather than from ~/quicklisp/local-projects.

(pushnew (merge-pathnames "local-projects/clamps/packages/svg-import-export/"
                          ql:*quicklisp-home*)
         asdf:*central-registry*)

(pushnew (merge-pathnames "local-projects/clamps/packages/clog-dsp-widgets/" ql:*quicklisp-home*)
      asdf:*central-registry*)

(asdf:defsystem #:clamps
  :description "Common Lisp Aided Music Production System"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi
	       #:cl-ppcre
               #:cl-refs
               #:incudine
               #:cm
               #:fomus
               #:ats-cuda)
  :components ((:file "init")
               (:file "load-packages")
               (:file "cm-exports-extra")
               (:file "inkscape-export")
               (:file "package")
               (:file "display-automation")
               (:file "clamps-utils")
               (:file "clamps")
               (:file "make-doc")
               ))
