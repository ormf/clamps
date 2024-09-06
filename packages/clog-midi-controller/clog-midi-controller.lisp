;;;; clog-midi-controller.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(in-package #:clog-midi-controller)

(defclass clog-midi-controller ()
  ((midi-controller :initarg :midi-controller :accessor m-controller
                    :documentation "Accessor method for the /midi-controller/ slot of a <<clog-midi-controller>> instance.")
   (connection-hash-key :initarg :connection-hash-key :reader connection-hash-key
                        :documentation "Reader for the /connection-hash-key/ of a
<<clog-midi-controller>> instance to its Gui. This slot is for internal purposes."))
  (:documentation "Base Class for the Gui instance of a MIDI controller. Note that each
open Gui window will create a new instance of all clog-midi-controller
it contains and multiple instances of the same Gui page will all share
the same midi-controller instance. Any update of any Gui element in
any connected Gui will call the <<set-val>> function of the
corresponding <<ref-object>> in a slot of the midi-controller
instance, automatically triggering updates in all other connected
Guis.

clog-midi-controller implements the following slots:

=:midi-controller= -- The midi controller instance of cl-midictl, derived from its <<midi-controller>> class

=:connection-hash-key= -- A read-only slot containing the hash key of the web connection of the Gui instance.
"))

(defgeneric update-gui-state (clog-midi-controller))
