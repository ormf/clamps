;;;; package.lisp
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:cl-refs
  (:use #:cl)
  (:export
   #:make-ref #:set-val #:%set-val #:get-val
   #:make-bang #:trigger #:%trigger
   #:make-computed
   #:clear-dependencies #:with-updating-deps
   #:on-deps-update
   #:copy-ref
   ))
