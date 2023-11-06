;;;; clog-midi-controller.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(in-package #:clog-midi-controller)

(defclass clog-midi-controller ()
  ((midi-controller :initarg :midi-controller :accessor midi-controller)))
