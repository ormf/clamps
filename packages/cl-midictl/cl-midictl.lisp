;;; 
;;; cl-midictl.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2018 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

;;; midictl handles the midi infrastructure: It uses the hash table
;;; *midi-controllers* to (un)register controllers by their id and
;;; additionally pushes/removes all controllers to/from (gethash
;;; *midi-controllers* 'input). It also starts the midi-responder
;;; which accepts midi-events and dispatches them (by midi-channel) to
;;; all appropriate controllers. In addition the midi-responder
;;; maintains *midi-cc-state* which contains the cc-state of the
;;; hardware controllers attached.
;;;
;;; NOTE: The values of *midi-cc-state* should only be altered by the
;;; existing midi-responder and otherwise only read to obtain
;;; information about the state of the hardware controllers.
;;;
;;; The controller instances generally contain model-slots to capture
;;; the state of the controller, but there might be differences
;;; between the controller and the hardware state as the controller
;;; state could be altered by gui interaction or preset loading,
;;; etc. The code in the ref-set-hooks of the controller instances
;;; should take care of handling the behaviour in case the hardware
;;; and the gui get out of sync (like colouring the gui accordingly
;;; and "catching" fader values sent from the controller).
;;;
;;; One main purpose of the *midi-controllers* infrastructure is to
;;; guarantee that there are no problems detaching/reattaching
;;; hardware devices after instantiating gui objects or such (in the
;;; future we might even implement reacting to udev events by
;;; instantiating/removing gui-instances on the fly).
;;;

(in-package :cl-midictl)

;;; utilities
(defparameter *midi-debug* nil)

(defconstant +ml-opcode-mask+ #xf0)
(defconstant +ml-channel-mask+ #x0f)
(defparameter +ml-note-off-opcode+ 8)
(defparameter +ml-note-on-opcode+ 9)
(defparameter +ml-control-change-opcode+ 11)
(defparameter +ml-channel-pressure-opcode+ 13)
(defparameter +ml-pitch-bend-opcode+ 14)
(defparameter +ml-key-pressure-opcode+ 10)
(defparameter +ml-program-change-opcode+ 12)
(defparameter +ml-msg-sysex-type+ 15)
(defparameter +ml-msg-eox-type+ 127)

#|
(defparameter +ml-default-note-on-velocity+ 64)
(defparameter +ml-default-note-off-velocity+ 64)
(defparameter +ml-msg-sysex-type+ 15)
(defparameter +ml-msg-mtc-quarter-frame-type+ 31)
(defparameter +ml-msg-song-position-type+ 47)
(defparameter +ml-msg-song-select-type+ 63)
(defparameter +ml-msg-cable-select-type+ 95)
(defparameter +ml-msg-tune-request-type+ 111)
(defparameter +ml-msg-eox-type+ 127)
(defparameter +ml-msg-timing-clock-type+ 143)
(defparameter +ml-msg-timing-tick-type+ 159)
(defparameter +ml-msg-start-type+ 175)
(defparameter +ml-msg-continue-type+ 191)
(defparameter +ml-msg-stop-type+ 207)
(defparameter +ml-msg-active-sensing-type+ 239)
(defparameter +ml-msg-system-reset-type+ 255)
(defparameter +ml-meta-type+ 0)
|#

(defparameter *ml-opcodes*
  `((,+ml-control-change-opcode+ . :cc)
    (,+ml-note-on-opcode+ . :note-on)
    (,+ml-note-off-opcode+ . :note-off)
    (,+ml-program-change-opcode+ . :pgm-change)
    (,+ml-pitch-bend-opcode+ . :pitch-bend)
    (,+ml-key-pressure-opcode+ . :key-pressure)
    (,+ml-channel-pressure-opcode+ . :channel-pressure)
    (,+ml-msg-sysex-type+ . :sysex)))

(defun status->opcode (st)
  (cdr (assoc (ash (logand st +ml-opcode-mask+) -4)
              *ml-opcodes*)))

(defun status->channel (st)
  (logand st +ml-channel-mask+))

(declaim (inline rotary->inc))
(defun rotary->inc (num)
  (if (> num 63)
      (- num 128)
      num))

(declaim (inline clip))
(defun clip (val min max)
  (min (max val min) max))

(defmacro rotary->cc (array ch d1 d2)
  `(setf (aref ,array ,ch ,d1)
         (clip (+ (aref ,array ,ch ,d1)
                  (rotary->inc ,d2))
               0 127)))

(declaim (inline midi-out))
(defun midi-out (stream status data1 data2)
  "create a closure to defer a call to jm_write_event."
  (lambda ()
    (osc-midi-write-short stream status data1 data2)))

(declaim (inline ctl-out))
(defun ctl-out (stream ccno ccval chan)
  "wrapper for midi ctl-change messages."
  (let ((status (+ chan (ash #b1011 4))))
    (midi-out stream status ccno ccval)))

(defun add-midi-cc-fn (fn channel ccnum)
  "Add /fn/ to <<*midi-cc-fns*>> for /channel/ and /ccnum/.
/fn/ will be called with the controller value as argument when MIDI
input at /channel/ and /ccnum/ is received.

@Arguments
fn - Function of one Argument to call on MIDI input
channel - Integer in the range [1..16] denoting the MIDI channel.
ccnum - Integer in the range [1..128] denoting the MIDI Controller number.

@Example
(add-midi-cc-fn
  (lambda (cc-val) (format t \"Received Controller Value ~a~%\" cc-val))
  1 1)
;; => (#<function (lambda (cc-val)) {564DA61B}>)

@See-also
*midi-cc-fns*
remove-all-channel-midi-cc-fns
remove-all-midi-cc-fns
remove-midi-cc-fns
show-midi-cc-fns
"
  (push fn (aref (aref *midi-cc-fns* (1- channel)) (1- ccnum))))

(defun show-midi-cc-fns (channel ccnum)
  "Show all functions stored in *midi-cc-fns* for /channel/ and /ccnum/.

@Arguments
channel - Integer in the range [1..16] denoting the MIDI channel.
ccnum - Integer in the range [1..128] denoting the MIDI Controller number.

@Example
(show-midi-cc-fns 1 1) ; => nil

;; Output in the REPL:
;; cc-fns of channel 1, ccnum 1: (#<function (lambda (ccval)) {564E441B}>)

@See-also
add-midi-cc-fn
*midi-cc-fns*
remove-midi-cc-fns
remove-all-channel-midi-cc-fns
remove-all-midi-cc-fns
"
  (format t "cc-fns of channel ~a, ccnum ~a: ~a~%" channel ccnum
        (aref (aref *midi-cc-fns* (1- channel)) (1- ccnum))))

(defun remove-midi-cc-fns (channel ccnum &optional (midi-port *default-midi-port*))
  "Remove all functions from *midi-cc-fns* for /channel/ and /ccnum/.

@Arguments
channel - Integer in the range [1..16] denoting the MIDI channel.
ccnum - Integer in the range [1..128] denoting the MIDI Controller number.

@Example
(remove-midi-cc-fns 1 1) ; => nil

@See-also
add-midi-cc-fn
*midi-cc-fns*
remove-all-channel-midi-cc-fns
remove-all-midi-cc-fns
show-midi-cc-fns
"
  (setf (aref (aref (midi-port-cc-fns midi-port) (1- channel)) (1- ccnum)) nil))

(defun remove-all-channel-midi-cc-fns (channel &optional (midi-port *default-midi-port*))
  "Remove all functions from *midi-cc-fns* for all ccnums of /channel/ of /midi-port/.

@Arguments
channel - Integer in the range [1..16] denoting the MIDI channel.

@Example
(remove-all-channel-midi-cc-fns 1) ; => nil

@See-also
add-midi-cc-fn
*midi-cc-fns*
remove-midi-cc-fns
remove-all-midi-cc-fns
show-midi-cc-fns
"
  (dotimes (ccnum 128)
    (remove-midi-cc-fns (1- channel) (1+ ccnum) midi-port)))

(defun remove-all-midi-cc-fns (&optional (midi-port *default-midi-port*))
  "Remove all functions from *midi-cc-fns* for all ccnums and channels.

@Example
(remove-all-midi-cc-fns) ; => nil

@See-also
add-midi-cc-fn
*midi-cc-fns*
remove-midi-cc-fns
remove-all-channel-midi-cc-fns
show-midi-cc-fns
"
  (dotimes (channel 16)
    (dotimes (ccnum 128)
      (remove-midi-cc-fns (1+ channel) (1+ ccnum) midi-port))))

(defparameter *midi-controllers* (make-hash-table :test #'equal)
  "hash-table which stores all currently active midi controllers by id
  and an entry for all used midi-ins of the active controllers by
  pushing the controller instance to the 'midi-in entry of this
  hash-table. Maintenance of *midi-controllers* is done within the
  midi-controller methods.")

;;; class def

(defclass midi-controller ()
  ((id :initform nil :initarg :id :accessor mctl-id)
;;;   (gui-update-off :initform nil :accessor gui-update-off)
;;;   (gui :initform nil :accessor mc-gui)
   (chan :initform nil :initarg :chan :accessor chan :type (or null (integer 1 16))
         :documentation
         "Accessor method for the chan slot of an instance of type
<<midi-controller>>.")
   (cc-map :initform (make-array 128 :initial-element nil)
           :initarg :cc-map :accessor cc-map
           :documentation
         "Accessor method for the cc-map slot of an instance of type
<<midi-controller>>.")
   (keynum-map :initform (make-array 128 :initial-element nil)
           :initarg :keynum-map :accessor keynum-map
           :documentation
         "Accessor method for the keynum-map slot of an instance of type
<<midi-controller>>.")
   (midi-port :initform nil :initarg :midi-port :accessor midi-port
               :documentation
               "Accessor method for the midi-port slot of an instance of type
<<midi-controller>>.")
   (midi-input :initform nil :initarg :midi-input :accessor midi-input
               :documentation
               "Accessor method for the midi-input slot of an instance of type
<<midi-controller>>.")
   (midi-output :initform nil :initarg :midi-output :accessor midi-output
                :documentation
                "Accessor method for the midi-output slot of an instance of type
<<midi-controller>>.")
   (echo :initarg :echo :initform t :accessor echo
         :documentation "Accessor method for the echo slot of an instance of type
<<midi-controller>>. En/disable direct updates in hw-controller when
midi-input is received. (default: t)")
   (last-note-on :initform 0 :initarg :last-note-on :accessor last-note-on
                 :documentation
                 "Accessor method for the last-note-on slot of an instance of type <<midi-controller>>.")
   (cc-state :initform (make-array 128 :initial-contents (loop for i below 128 collect (make-bang (lambda ()) 0)))
             :initarg :cc-state :accessor cc-state
             :documentation
                 "Accessor method for the cc-state slot of an instance of type <<midi-controller>>.")
   (note-state :initform (make-array 128 :initial-contents (loop for i below 128 collect (make-bang (lambda ()) 0)))
               :initarg :note-state :accessor note-state
               :documentation
                 "Accessor method for the note-state slot of an instance of type <<midi-controller>>.")
   ;;; storage of functions to call for individual cc events. An entry
   ;;; of the array is a list of functions accepting one arg: the
   ;;; controller value. Instead of cc-fns, the ref-objects of cc-state can be used.
   (cc-fns :initform (make-array 128 :initial-element nil)
           :initarg :cc-fns :accessor cc-fns)
   ;;; storage of functions to call for individual note-on/off
   ;;; events. This is a list of functions accepting two args: keynum
   ;;; and velo.
   (note-fns :initform nil :initarg :note-fns :accessor note-fns)
   (unwatch :initform nil :initarg :unwatch :accessor unwatch))
  (:documentation "Generic base class for midi controllers in the /cl-midictl/
package. An instance of a class derived from /midi-controller/ should
get initialized with <<add-midi-controller>> and removed with
<<remove-midi-controller>> in order to add/remove it to/from the midi
controller registry.

midi-controller implements the following slots with initargs
being the keywords of the slot symbol:

=cc-map= -- Array mapping CC nums to internal indexes of the instance.

=keynum-map= -- Array mapping keynums to internal indexes of the instance.

=cc-fns= -- Array of 128 lists storing functions to call when
receiving a value at any of the 128 CC numbers.

=cc-state= -- Array of 128 <<bang-object><bang-objects>> storing the last
received CC value for each CC number.

=chan= -- Integer in the range [1..16] denoting the MIDI channel.

=echo= -- Boolean to en/disable echoing of midi input to midi output.

=id= -- Keyword or Symbol to identify the controller in the registry.

=last-note-on= -- The keynum of the last received note-on event with positive
velocity.

=midi-input= -- jackmidi:input-stream for MIDI input.

=midi-output= -- jackmidi:output-stream for MIDI output.

=note-fns= -- Array of 128 lists storing functions to call with the
velocity as argument, mapped to a received note-on event on any of the
128 keynumbers.

=note-state= -- Array of 128 <<ref-object><ref-objects>> storing the last
received velocity for each keynum.

=unwatch= -- Storage for unwatch functions for the slots of the
controller instance, handled internally.

@See-also
add-midi-controller
find-controller
remove-midi-controller
remove-all-midi-controllers
"))

(defmethod initialize-instance :after ((instance midi-controller) &rest args)
  (declare (ignorable args))
  (with-slots (id midi-port midi-input midi-output chan) instance
;;    (format t "~&midictl-id: ~a ~%" id)
    (if (gethash id *midi-controllers*)
        (warn "id already used: ~a" id)
        (progn
          (if (keywordp midi-port) (setf midi-port (find-midi-port midi-port)))
          (unless chan (setf chan *default-midi-channel*))
          (setf midi-input (or midi-input (midi-port-in (or midi-port *default-midi-port*))))
          (format t "adding midi controller ~S~%" id)
          (setf midi-output (ensure-default-midi-out (or midi-output (midi-port-out (or midi-port *default-midi-port*)))))
          (when id
            (push instance (gethash midi-input *midi-controllers*))
            (setf (gethash id *midi-controllers*) instance))))))

(defgeneric cleanup (instance)
  (:method ((instance midi-controller)))
  (:documentation "Cleanup function for a midi-controller before it gets deleted."))

(defgeneric (setf midi-input) (new-midi-in instance)
  (:method (new-midi-in (instance midi-controller))
    (if (member instance (gethash (midi-input instance) *midi-controllers*))
        (setf (gethash (midi-input instance) *midi-controllers*)
              (delete instance (gethash (midi-input instance) *midi-controllers*)))
        (warn "couldn't remove midi-controller ~a" instance))
    (setf (slot-value instance 'midi-in) new-midi-in)
    (push instance (gethash new-midi-in *midi-controllers*)))
  (:documentation
   "Set the midi-input slot of /instance/ to /new-midi-in/ and update
*midi-controllers*."))

(defgeneric handle-midi-in (instance opcode channel d1 d2)
  (:documentation
   "midi-handling of a midi-controller. This is called by the midi
receiver but can also be called by gui code or directly to emulate
controller actions."))

(defmethod handle-midi-in ((instance midi-controller) opcode channel d1 d2)
  (with-slots (chan cc-fns cc-map keynum-map cc-state note-state pitch-bend-state after-touch-state
               note-fns pitch-bend-fns after-touch-fns last-note-on)
      instance
    (incudine.util:msg :debug "midi-controller-handle-midi-in: ~a ~a ~a" opcode d1 d2)
    (case opcode
      (:cc (if (and (= chan (1+ channel)) (aref cc-map d1))
               (progn
                 (set-val (aref cc-state (aref cc-map d1)) d2)
                 (mapcar (lambda (fn) (funcall fn d2)) (aref cc-fns d1)))
               (warn "ccnum ~d not mapped in midi-controller ~a, ignored." d1 instance)))
      (:note-on (if (and (= chan (1+ channel)) (aref keynum-map d1))
                    (progn
                      (set-val (aref note-state (aref keynum-map d1)) d2)
                      (mapcar (lambda (fn) (funcall fn d1 d2)) (note-fns instance))
                      (setf last-note-on d1))
                    (warn "note-on ~d not mapped in midi-controller ~a, ignored." d1 instance)))
      (:note-off (if (and (= chan (1+ channel)) (aref keynum-map d1))
                     (progn
                       (set-val (aref note-state (aref keynum-map d1)) 0)
                       (mapcar (lambda (fn) (funcall fn d1 0)) (note-fns instance)))
                     (warn "note-off ~d not mapped in midi-controller ~a, ignored." d1 instance)))
      (:pitch-bend (progn
                     (set-val (aref pitch-bend-state channel) (+ d1 (ash d2 7)))
                     (mapcar (lambda (fn) (funcall fn (+ d1 (ash d2 7)))) (aref pitch-bend-fns channel))))
      (:channel-pressure (progn
                           (set-val (aref after-touch-state channel) d1)
                           (mapcar (lambda (fn) (funcall fn d1)) (aref after-touch-fns channel)))))))

;;; (make-instance 'midi-controller)

(defgeneric update-hw-state (instance))

(defmethod update-hw-state ((instance midi-controller))
  "Set the state of the hardware of <<midi-controller>> /instance/
according to <<*midi-cc-state*>> by sending all 128 cc values of the
controllers midi-channel to the midi-controller's midi output."
  (with-slots (chan midi-output) instance
    (dotimes (cc-num 128)
      (osc-midi-write-short
       midi-output
       (+ (1- chan) 176) cc-num (get-val (aref (aref *midi-cc-state* chan) cc-num))))))

(defun ensure-defaultmidi-out (midi-out)
  (or midi-out *midi-out1*))

;;; central registry for midi controllers:

(defun add-midi-controller (class id &rest args)
  "Register a MIDI controller of class /class/ with ID /id/ and optional
initialization argumens /args/.

@Arguments
class - The class of the midi controller to add.
id - Keyword or Symbol used as ID of the instance.
args - Initialization arguments appropriate for the class.

@See-also
list-midi-controllers
find-controller
midi-controller
remove-midi-controller
remove-all-midi-controllers
"
  (if (find-controller id)
      (error "midi-controller ~S already defined!" id)
      (apply #'make-instance class :id id args)))

(defun remove-midi-controller (id)
  "Unregister and delete the instance of a midi controller with ID /id/.

@Arguments
id - Keyword or Symbol used as ID of the instance.    

@See-also
add-midi-controller
list-midi-controllers
find-controller
midi-controller
remove-all-midi-controllers

"  (let ((instance (gethash id *midi-controllers*)))
    (if instance
        (progn
          (mapc #'funcall (unwatch instance))
          (if (or (not (midi-input instance)) (member instance (gethash (midi-input instance) *midi-controllers*)))
              (progn
                (cleanup instance)
                (setf (gethash (midi-input instance) *midi-controllers*)
                      (delete instance (gethash (midi-input instance) *midi-controllers*)))
                (format t "removing midi controller ~S (~a)~%" id (remhash id *midi-controllers*)))
              (warn "couldn't remove midi-controller ~a" instance))))))

(defun remove-all-midi-controllers ()
  "Unregister and delete all currently registered MIDI controller instances.

@See-also
add-midi-controller
list-midi-controllers
find-controller
midi-controller
remove-midi-controller
"
  (mapc #'remove-midi-controller (list-midi-controllers))
  nil)

(defun find-controller (id)
  "Return MIDI controller instance with ID /id/ or /nil/ if not
registered.

@Arguments
id - Keyword or Symbol used as ID of a midicontroller instance .

@See-also
add-midi-controller
list-midi-controllers
midi-controller
remove-midi-controller
remove-all-midi-controllers

"  (let ((controller (gethash id *midi-controllers*)))
     (if controller
         controller)))

(defun list-midi-controllers ()
  "Return the IDs of all registered midi controllers in a list.

@See-also
add-midi-controller
find-controller
midi-controller
remove-midi-controller
remove-all-midi-controllers

"  (let (acc)
     (maphash (lambda (key value)
                (declare (ignore value))
                (if (member (type-of key) '(symbol keyword))
                    (push key acc)))
              *midi-controllers*)
     (sort acc #'string< :key #'symbol-name)))

;;; (ensure-controller :nk2)
;;; (setf *midi-debug* nil)

(defgeneric generic-midi-handler (midi-port opcode d1 d2 channel))

(defmethod generic-midi-handler ((port midi-port) opcode d1 d2 channel)
  "Maintain the midi-cc-state and midi-note-state arrays and calls all
functions registered in midi-cc-fns or midi-note-fns of the specified
midi-port struct.

The function gets called by the midi responder installed with
start-midi-receive before calling the the handle-midi-in method of all
controllers registered with the port.

Normally controllers will install their own handlers using the
handle-midi-in method.

But it can be used e.g. in a scratch environment where no controller
is actually instanced.

In addition this is used in the nanoktl2-gui code (in the
:clog-midi-controller package) to compare the real state of the
hardware controller's faders to the cc-state being set directly in the
controller instance using mouse interaction or presets and only
updating the cc-state in the controller when the incoming midi values
agree to the values to avoid jumps in the cc-state of the controller
instance."
  (with-slots (id cc-state note-state pitch-bend-state after-touch-state
               cc-fns note-fns pitch-bend-fns after-touch-fns)
      port
    
    (incudine.util:msg :debug "generic-midi-handler: ~S ~a ~a ~a" id opcode d1 d2 (1+ channel))
    (case opcode
      (:cc
       (set-val (aref (aref cc-state channel) d1) d2)
       (dolist (fn (aref (aref cc-fns channel) d1)) (funcall fn d2)))
      (:note-on
       (set-val (aref (aref note-state channel) d1) d2)
       (dolist (fn (aref (aref note-fns channel) d1)) (funcall fn d2)))
      (:note-off
       (set-val (aref (aref note-state channel) d1) 0)
       (dolist (fn (aref (aref note-fns channel) d1)) (funcall fn 0)))
      (:pitch-bend
       (let ((bendval (+ d1 (ash d2 7))))
         (set-val (aref pitch-bend-state channel) bendval)
         (dolist (fn (aref pitch-bend-fns channel) d1) (funcall fn bendval))))
      (:channel-pressure
       (let ((after-touch-val d2))
         (set-val (aref after-touch-state d1) after-touch-val)
         (dolist (fn (aref (aref after-touch-fns channel) d1)) (funcall fn after-touch-val)))))))

(defun start-midi-receive (midi-port)
  "Start the clamps generic midi handler and all registered MIDI responders
of /midi-port/.

@Arguments
midi-port - Instance of a midi-port struct.

@See-also
stop-midi-receive
"
  (incudine.util:msg :info "removing-responders")
  (let ((input (midi-port-in midi-port)))
    (remove-all-responders input)
    (make-responder input
                    (lambda (st d1 d2)
                      (let ((chan (status->channel st))
                            (opcode (status->opcode st)))
                        (generic-midi-handler midi-port opcode d1 d2 chan)
                        (dolist (controller (gethash input *midi-controllers*))
                          (declare (type midi-controller controller))
                          (handle-midi-in controller opcode chan d1 d2)))))
    (recv-start input)
    (update-all-controllers midi-port)
    :midi-rcv-started))

;;; (start-midi-receive *midi-in1*)

(defun stop-midi-receive (midi-port)
  "Stop the clamps generic midi handler and remove all registered MIDI
responders of input stream /input/.

@Arguments
midi-port - Instance of a midi-port struct.

@See-also
start-midi-receive
"
  (let ((input (midi-port-in midi-port)))
    (remove-all-responders input)
    (recv-stop input)))

;;; (stop-midi-receive *midi-in1*)

(defun update-all-controllers (midi-port)
  "call <<update-hw-state>> on all registered midi-controllers of
/midi-port/."
  (dolist (controller (gethash midi-port *midi-controllers*))
    (update-hw-state controller)))

;;; (start-midi-engine)
