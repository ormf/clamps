;;;; of-incudine-dsps.asd

(asdf:defsystem #:of-incudine-dsps
  :description "Package with incudine dsps."
  :author "<orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license "Public domain, use at own risk, no warranties whatsoever."
  :serial t
  :depends-on (:orm-utils :cm-incudine)
  :components ((:file "package")
               (:file "utils")
               (:file "buffer-stretch-play")
               (:file "lsample")))

