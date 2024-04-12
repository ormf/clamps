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
    (,+ml-channel-pressure-opcode+ . :channel-pressure)))

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

(defparameter *midi-controllers* (make-hash-table :test #'equal)
  "hash-table which stores all currently active midi controllers by id
  and an entry for all used midi-ins of the active controllers by
  pushing the controller instance to the 'midi-in entry of this
  hash-table. Maintenance of *midi-controllers* is done within the
  midi-controller methods.")

;;; class def

(defclass midi-controller ()
  ((id :initform nil :initarg :id :accessor id)
   (gui-update-off :initform nil :accessor gui-update-off)
   (gui :initform nil :accessor gui)
   (chan :initform 0 :initarg :chan :accessor chan)
   (cc-map :initform (make-array 128 :initial-contents (loop for i below 128 collect i))
           :initarg :cc-map :accessor cc-map)
   (midi-input :initform nil :initarg :midi-input :accessor midi-input)
   (midi-output :initform nil :initarg :midi-output :accessor midi-output)
   (echo :initarg :echo :initform t :accessor echo
         :documentation "en/disable direct updates in hw-controller when midi-input is received. (default: t)")
   (last-note-on :initform 0 :initarg :last-note-on :accessor last-note-on)
   (cc-state :initform (make-array 128 :initial-contents (loop for i below 128 collect (make-ref 0)))
             :initarg :cc-state :accessor cc-state)
   (note-state :initform (make-array 128 :initial-contents (loop for i below 128 collect (make-ref 0)))
             :initarg :note-state :accessor note-state)
   ;;; storage of functions to call for individual cc events. An entry
   ;;; of the array is a list of functions accepting one arg: the
   ;;; controller value.
   (cc-fns :initform (make-array 128 :initial-element nil)
           :initarg :cc-fns :accessor cc-fns)
   ;;; storage of functions to call for individual note-on/off
   ;;; events. This is a list of functions accepting two args: keynum
   ;;; and velo.
   (note-fns :initform nil :initarg :note-fns :accessor note-fns)
   (unwatch :initform nil :initarg :unwatch :accessor unwatch))
  (:documentation "generic class for midi-controllers. An instance
  should get initialized with #'add-midi-controller and get removed
  with #'remove-midi-controller, using its id as argument in order to
  close the gui and remove its handler functions from
  *midi-controllers*."))

(defgeneric (setf midi-input) (new-midi-in instance)
  (:method (new-midi-in (instance midi-controller))
    (if (member instance (gethash (midi-input instance) *midi-controllers*))
        (setf (gethash (midi-input instance) *midi-controllers*)
              (delete instance (gethash (midi-input instance) *midi-controllers*)))
        (warn "couldn't remove midi-controller ~a" instance))
    (setf (slot-value instance 'midi-in) new-midi-in)
    (push instance (gethash new-midi-in *midi-controllers*)))
  (:documentation
   "set the midi-input slot of instance to new-midi-in and update *midi-controllers*"))

(defgeneric handle-midi-in (instance opcode d1 d2)
  (:documentation
   "midi-handling of a midi-controller. This is called by the midi
receiver but can also be called by gui code or directly to emulate
controller actions."))

(defmethod handle-midi-in ((instance midi-controller) opcode d1 d2)
  (with-slots (cc-fns cc-map cc-state note-state note-fn last-note-on) instance
    (format t "midi-controller-handle-midi-in~%")
    (case opcode
      (:cc (progn
             (set-val (aref cc-state (aref cc-map d1)) d2)
             (mapcar (lambda (fn) (funcall fn d2)) (aref cc-fns d1))))
      (:note-on (progn
                  (set-val (aref note-state (aref cc-map d1)) d2)
                  (mapcar (lambda (fn) (funcall fn d1 d2)) (note-fns instance))
                  (setf last-note-on d1)))
      (:note-off (progn
                   (set-val (aref note-state (aref cc-map d1)) 0)
                   (mapcar (lambda (fn) (funcall fn d1 0)) (note-fns instance)))))))

;;; (make-instance 'midi-controller)

(defgeneric update-state (instance)
  (:documentation "set state of controller according to *midi-cc-state*"))

(defmethod update-state ((instance midi-controller))
  (with-slots (chan midi-output) instance
    (dotimes (cc-num 128)
      (osc-midi-write-short
       midi-output
       (+ chan 176) cc-num (get-val (aref (aref *midi-cc-state* chan) cc-num))))))

(defmethod initialize-instance :after ((instance midi-controller) &rest args)
  (declare (ignorable args))
  (with-slots (id midi-input midi-output) instance
;;    (format t "~&midictl-id: ~a ~%" id)
    (if (gethash id *midi-controllers*)
        (warn "id already used: ~a" id)
        (progn
          (format t "adding controller ~S~%" id)
          (unless midi-input (error "no midi-input specified for ~a" instance))
          (unless midi-output (error "no midi-output specified for ~a" instance))
;;;          (setf midi-output (cm:ensure-jackmidi midi-output))
          (push instance (gethash midi-input *midi-controllers*))
          (setf (gethash id *midi-controllers*) instance)))))

;;; central registry for midi controllers:

(defun add-midi-controller (class &rest args)
  "register midi-controller by id and additionally by pushing it onto
the hash-table entry of its midi-input."
  (apply #'make-instance class args))

(defun remove-midi-controller (id)
  (let ((instance (gethash id *midi-controllers*)))
    (format t "~&removing: ~a~%" id)
    (if instance
        (progn
          (mapcar #'funcall (unwatch instance))
          (if (member instance (gethash (midi-input instance) *midi-controllers*))
              (progn
                (setf (gethash (midi-input instance) *midi-controllers*)
                      (delete instance (gethash (midi-input instance) *midi-controllers*)))
                (format t "removing ~S: ~a" id (remhash id *midi-controllers*)))
              (warn "couldn't remove midi-controller ~a" instance))))))

(defun remove-all-midi-controllers ()
  (loop
    for key being the hash-keys of *midi-controllers*
    for v being the hash-values of *midi-controllers*
    do
       (unless (consp v)
         (format t "~&removing: ~a~%" v)
         (if v
             (if (member v (gethash (midi-input v) *midi-controllers*))
                 (progn
                   (setf (gethash (midi-input v) *midi-controllers*)
                         (delete v (gethash (midi-input v) *midi-controllers*)))
                   (format t "removing ~a: ~a" key (remhash key *midi-controllers*)))
                     (warn "couldn't remove midi-controller ~a" v))))))

(defun find-controller (id)
  (gethash id *midi-controllers*))

(defun ensure-controller (id)
  (let ((controller (gethash id *midi-controllers*)))
    (if controller
        controller
        (error "controller ~S not found!" id))))

;;; (ensure-controller :nk2)
;;; (setf *midi-debug* nil)

(defun generic-midi-handler (opcode d1 d2 channel)
  "the generic handler simply maintains the *midi-cc-state* array and
calls all functions registered in *midi-cc-fns*.

The function gets called by the midi responder installed with
start-midi-receive before calling the the handle-midi-in method of all
registered controllers.

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
  (incudine.util:msg :debug "~S ~a ~a ~a" opcode d1 d2 channel)
  (case opcode
    (:cc
     (setf (aref (aref *midi-cc-state* channel) d1) d2)
     (dolist (fn (aref (aref *midi-cc-fns* channel) d1)) (funcall fn d2)))
    (:note-on
     (setf (aref (aref *midi-note-state* channel) d1) d2)
     (dolist (fn (aref (aref *midi-note-fns* channel) d1)) (funcall fn d2)))
    (:note-off
     (setf (aref (aref *midi-note-state* channel) d1) 0)
     (dolist (fn (aref (aref *midi-note-fns* channel) d1)) (funcall fn 0)))))

(defun start-midi-receive (input)
  "general receiver/dispatcher for all midi input of input arg. On any
midi input it scans all elems of *midi-controllers* and calls their
handle-midi-in method in case the event's midi channel matches the
controller's channel."
  (incudine.util:msg :info "removing-responders")
  (remove-all-responders input)
  (make-responder input
     (lambda (st d1 d2)
       (let ((chan (status->channel st))
             (opcode (status->opcode st)))
         (generic-midi-handler opcode d1 d2 chan)
         (dolist (controller (gethash input *midi-controllers*))
           (declare (type midi-controller controller))
           (if (= chan (chan controller))
               (handle-midi-in controller opcode d1 d2))))))
  (recv-start input)
  (update-all-controllers input)
  :midi-rcv-started)

;;; (start-midi-receive *midi-in1*)

(defun stop-midi-receive (input)
  "remove all responders of input and stop general receiver/dispatcher
of the input."
  (remove-all-responders input)
  (recv-stop input))

;;; (stop-midi-receive *midi-in1*)

(defun update-all-controllers (midi-in-port)
  (dolist (controller (gethash midi-in-port *midi-controllers*))
    (update-state controller)))

;;; (start-midi-engine)

(defun start-midi-engine ()
  "open midi ports and start realtime thread."
  (when *midi-in1*
    (recv-stop *midi-in1*)
    (remove-all-responders *midi-in1*)
    (jackmidi:close *midi-in1*))
  (incudine.util:msg :warn "closing midi streams~%")
  (mapcar #'jackmidi:close jackmidi::*streams*)
  (sleep 0.1)
  (when *midi-out1* (jackmidi:close *midi-out1*))
  (setf *midi-in1* (jackmidi:open :direction :input
                                     :port-name "midi_in_1"))
  (loop repeat 20 until *midi-in1* do (progn
                                        (incudine.util:msg :warn "waiting for *midi-in1*")
                                        (sleep 0.1)))
  (setf *midi-out1* (jackmidi:open :direction :output
                                   :port-name "midi_out_1"))
  (loop repeat 20 until *midi-out1* do (progn
                                        (incudine.util:msg :warn "waiting for *midi-out1*")
                                        (sleep 0.1)))
  (start-midi-receive *midi-in1*)
;;;  (incudine:rt-start)
  (if (and *midi-in1* *midi-out1*)
      (progn
        (incudine.util:msg :warn "~a" *midi-in1*)
        (incudine.util:msg :warn "~a" *midi-out1*)
        (list *midi-in1* *midi-out1*))
      (error "midi didn't start properly")))

