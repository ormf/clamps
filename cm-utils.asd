;;;; cm-utils.asd

(asdf:defsystem #:cm-utils
  :description "Utilities for Common Music 2"
  :author "Orm Finnendahl <orm.finnendahl@selma-hfmdk-frankfurt.de>"
  :license "LLGPL"
  :serial t
  :depends-on (#:incudine
               #:orm-utils
               #:cl-ppcre
               #:cl-coroutine
               #:cm)
  :components ((:file "package")
               (:file "cm-utils")))

