;;;; cm-all.asd

(if (find-package :slynk) (pushnew :slynk *features*))
(if (find-package :swank) (pushnew :swank *features*))

(asdf:defsystem #:cm-all
  :description "cm with all additions loaded"
  :author "Orm Finnendahl <ormfinnendahl@selma.hfmdk-frankfurt.de"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on (
               #:cm
               )
  :components ((:module "src"
                :serial t
                :components
                (
                 (:file "package")
;;;                 (:file "cm-all")
                 
                 ))))
