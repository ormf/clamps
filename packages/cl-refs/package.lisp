;;;; package.lisp
;;
;;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>

(defpackage #:cl-refs
  (:use #:cl)
  (:export
   #:REF-OBJECT-SUPER
   #:*REFS-SEEN*
   #:REF-OBJECT #:BANG-OBJECT
   #:TRIGGER-FNS
   #:REF-LISTENERS
   #:REF-ID
   #:MAKE-REF #:SET-VAL #:%SET-VAL #:GET-VAL
   #:SETTER #:GETTER
   #:MAKE-BANG #:TRIGGER #:%TRIGGER
   #:MAKE-COMPUTED
   #:WATCH
   #:CLEAR-DEPENDENCIES #:WITH-UPDATING-DEPS
   #:ON-DEPS-UPDATE
   #:WITH-UNWATCHED
   #:COPY-REF
   #:TRIGGER
   #:%TRIGGER
   #:REMOVE-TRIGGER-FN
   #:REMOVE-ALL-TRIGGERS
   #:ADD-TRIGGER-FN
   #:TOGGLE-REF-FN
   #:ADD-WATCH
   #:UNWATCH-ALL
   ))
