;;;; clog-midi-controller.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(in-package #:clog-midi-controller)

(defclass clog-midi-controller ()
  ((midi-controller :initarg :midi-controller :accessor midi-controller)
   (connection-hash-key :initarg :connection-hash-key :accessor connection-hash-key)))

(defgeneric update-gui-state (clog-midi-controller))
