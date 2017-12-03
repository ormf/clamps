;;;; orm-utils.asd

(asdf:defsystem #:orm-utils
  :serial t
  :description "Utilities for personal work."
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "Public Domain, use at own risk, no liability whatsoever..."
  :depends-on (:cl-fad :cl-ppcre)
  :components ((:file "package")
               (:file "orm-utils")))

