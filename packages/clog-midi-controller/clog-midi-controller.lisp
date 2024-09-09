;;;; clog-midi-controller.lisp
;;
;;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>


(in-package #:clog-midi-controller)

(defclass clog-midi-controller ()
  ((midi-controller :initarg :midi-controller :accessor m-controller
                    :documentation "Accessor method for the /midi-controller/ slot of a <<clog-midi-controller>> instance.

@See-also
clog-midi-controller
")
   (connection-hash-key :initarg :connection-hash-key :reader connection-hash-key
                        :documentation "Reader for the /connection-hash-key/ of a
<<clog-midi-controller>> instance to its Gui. This slot is for internal purposes."))
  (:documentation "Base Class for the Gui instance of a MIDI controller. Note that each
open Gui window will create a new instance of all
clog-midi-controllers it contains. Multiple instances of the same
clog-midi-controller in different GUi windows will all share the same
midi-controller instance.

clog-midi-controller implements the following slots with initargs
being the keywords of the slot symbol:

=midi-controller= -- The midi controller instance of <<clamps:cl-midictl>>, its class derived from <<midi-controller>>.

=connection-hash-key= -- A read-only slot containing the hash key of the web connection of the Gui instance.

@Note
Any update of any Gui element in any connected Gui window will call
the <<set-val>> function of the corresponding <<ref-object>> in a slot
of the shared midi-controller instance, automatically triggering
updates of the same Gui Element in all other connected Gui windows.

@See-also
m-controller
"))

(defgeneric update-gui-state (clog-midi-controller))
