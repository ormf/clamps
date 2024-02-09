;;;; package.lisp
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:cl-refs
  (:use #:cl)
  (:export
   #:ref-object-super
   #:*refs-seen*
   #:*debug*
   #:ref-object #:bang-object
   #:ref-listeners
   #:ref-id
   #:make-ref #:set-val #:%set-val #:get-val
   #:make-bang #:trigger #:%trigger
   #:make-computed
   #:watch
   #:clear-dependencies #:with-updating-deps
   #:on-deps-update
   #:with-unwatched
   #:copy-ref
   ))
