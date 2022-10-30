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
;;; all appropriate controllers. One main purpose of the
;;; infrastructure is to guarantee that there are no problems
;;; detaching/reattaching hardware devices after instantiating the gui
;;; objects (in the future we might even implement reacting to udev
;;; events by instantiating/removing gui-instances on the fly).
;;;
;;; Further below is the definition of functions for player-access,
;;; default chans for the used midi-controllers/players, etc.

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
(defun midi-out (stream status data1 data2 data-size)
  "create a closure to defer a call to jm_write_event."
  (lambda ()
    (jackmidi:write-short stream (jackmidi:message status data1 data2) data-size)))

(declaim (inline ctl-out))
(defun ctl-out (stream ccno ccval chan)
  "wrapper for midi ctl-change messages."
  (let ((status (+ chan (ash #b1011 4))))
    (midi-out stream status ccno ccval 3)))

(defparameter *midi-controllers* (make-hash-table :test #'equal)
  "hash-table which stores all currently active midi controllers by id
  and an entry for all used midi-ins of the active controllers by
  pushing the controller instance to the 'midi-in entry of this
  hash-table. Maintenance of *midi-controllers* is done within the
  midi-controller methods.")

;;; class def

(defclass midi-controller ()
  ((id :initform nil :initarg :id :accessor id)
   (chan :initform 0 :initarg :chan :accessor chan)
   (cc-map :initform (make-array 128 :initial-contents (loop for i below 128 collect i))
           :initarg :cc-map :accessor cc-map)
   (gui :initform nil :initarg :gui :accessor gui)
   (midi-in :initform nil :initarg :midi-in :accessor midi-input)
   (midi-output :initform nil :initarg :midi-out :accessor midi-output)
   (last-note-on :initform 0 :initarg :last-note-on :accessor last-note-on)
   (cc-state :initform (make-array 128 :initial-element 0)
             :initarg :cc-state :accessor cc-state)
   ;;; storage of functions to call for individual cc events
   (cc-fns :initform (make-array 128 :initial-element #'identity)
           :initarg :cc-fns :accessor cc-fns)
   ;;; storage of functions to call for individual note-on events
   (note-fn :initform (lambda (keynum velo) (declare (ignore velo keynum)))
            :initarg :note-fn :accessor note-fn))
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
    (push instance (gethash new-midi-in *midi-controllers*))))

(defgeneric handle-midi-in (instance opcode d1 d2)
  (:documentation
   "define midi-handlers by simulating the appropriate mouse/keyboard interaction."))

(defmethod handle-midi-in ((instance midi-controller) opcode d1 d2)
  (with-slots (cc-fns cc-map cc-state note-fn last-note-on) instance
    (case opcode
      (:cc (progn
             (funcall (aref cc-fns d1) d2)
             (setf (aref cc-state (aref cc-map d1)) d2)))
      (:note-on (progn
                  (funcall (note-fn instance) d1 d2)
                  (setf last-note-on d1)))
      (:note-off (funcall (note-fn instance) d1 0)))))

;;; (make-instance 'midi-controller)

(defgeneric init-gui-callbacks (instance &key echo)
  (:documentation "initialize the gui callback functions."))

(defgeneric init-gui-callbacks (instance &key echo)
  (:documentation "initialize the gui callback functions of a
  controller. Called in initialize-instance :after."))

(defmethod init-gui-callbacks ((instance midi-controller) &key (echo t))
  (declare (ignore instance echo)))

(defmethod initialize-instance :after ((instance midi-controller) &rest args)
  (declare (ignorable args))
  (with-slots (id) instance
;;    (format t "~&midictl-id: ~a ~%" id)
    (if (gethash id *midi-controllers*)
        (warn "id already used: ~a" id)
        (progn
          (format t "adding controller ~a~%" id)
          (push instance (gethash (midi-input instance) *midi-controllers*))
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
        (if (member instance (gethash (midi-input instance) *midi-controllers*))
            (progn
              (setf (gethash (midi-input instance) *midi-controllers*)
                    (delete instance (gethash (midi-input instance) *midi-controllers*)))
              (format t "removing ~a: ~a" id (remhash id *midi-controllers*)))
            (warn "couldn't remove midi-controller ~a" instance)))))


(defun find-controller (id)
  (gethash id *midi-controllers*))

(defun ensure-controller (id)
  (let ((controller (gethash id *midi-controllers*)))
    (if controller
        controller
        (error "controller ~S not found!" id))))

;;; (ensure-controller :nk2)
;;; (setf *midi-debug* nil)

(defun start-midi-receive (input)
  "general receiver/dispatcher for all midi input of input arg. On any
midi input it scans all elems of *midi-controllers* and calls their
handle-midi-in method in case the event's midi channel matches the
controller's channel."
  (make-responder input
     (lambda (st d1 d2)
       (if *midi-debug*
           (format t "~&~S ~a ~a ~a~%" (status->opcode st) d1 d2 (status->channel st)))
       (let ((chan (status->channel st)))
         (dolist (controller (gethash input *midi-controllers*))
           (if (= chan (chan controller))
               (handle-midi-in controller (status->opcode st) d1 d2))))))
  (recv-start input))

(defun stop-midi-receive (input)
  "general receiver/dispatcher for all midi input of input arg. On any
midi input it scans all elems of *midi-controllers* and calls their
handle-midi-in method in case the event's midi channel matches the
controller's channel."
  (remove-all-responders input)
  (recv-stop input))
#|


(defvar *midi-dump-test*
           (make-responder *midiin*
             (lambda (st d1 d2)
               (cond ((jackmidi:sysex-message-p st)
                      (msg info "~S"
                           (jackmidi:input-stream-sysex-octets *midiin*)))
                     (t (msg info "~D ~D ~D" st d1 d2))))))

(recv-start *midiin*)
(recv-stop *midiin*)

;;;
;;;
;;;; Code für Luftstrom Controllers:
;;;
;;;

;;; *all-players* bezieht sich auf die Audio-Argumente (16 pro Player)


(defparameter *all-players* #(:auto :player1 :player2 :player3 :player4 :default))

(defparameter *controller-chans* '(:player1 0
                                   :player2 1
                                   :player3 2
                                   :player4 3
                                   :bs1 4
                                   :nk2 5))

(defparameter *player-lookup* nil)

(defun init-player-lookup ()
  (let ((hash (make-hash-table)))
    (loop for name across *all-players*
          for idx from 0
          do (if (consp name)
                 (mapcar (lambda (name) (setf (gethash name hash) idx)) name)
                 (setf (gethash name hash) idx))
             (setf (gethash idx hash) idx)
          finally (setf *player-lookup* hash))))

(init-player-lookup)

(declaim (inline player-aref))
(defun player-aref (idx-or-key)
  (or (gethash idx-or-key *player-lookup*)
      (error "no player named ~S" idx-or-key)))

;;; (player-aref :player1)
;;; (player-aref :default)

(defun player-name (idx)
  (aref *all-players* (player-aref idx)))

;;; (player-name :auto)

(declaim (inline controller-chan))
(defun controller-chan (idx-or-key)
  (or (getf *controller-chans* idx-or-key)
      (error "no controller named ~S" idx-or-key)))

;;; (controller-chan :default)


;;; Audio Argument Handling:
;;;
;;; *audio-preset-ctl-model* ist ein 5x16 Array von model-slots, das
;;; den State aller Audio Argumente der 5 player enthält.
;;;
;;; Aus Effizienzgründen wird beim Errechnen der Synth Parameter
;;; direkt aus dem Vektor *audio-preset-ctl-vector* gelesen, in dem
;;; die Werte des models in einem einfachen Vektor dupliziert
;;; sind. Das Setzen von Werten sollte *nicht* im Vektor, sondern im
;;; *audio-preset-ctl-model* vorgenommen werden, um die Synchronizität
;;; sämtlicher Werte in allen darauf referenzierenden Controllern zu
;;; gewährleisten.

(defparameter *audio-preset-ctl-vector*
  (let ((num-players 6) (num-args 16))
    (make-array (* num-players num-args)
                :element-type '(integer 0 127)
                :initial-element 0)))


(defparameter *audio-preset-ctl-model*
  (let* ((num-players 6) (num-args 16)
         (array-size (* num-players num-args)))
    (make-array array-size
                :element-type 'model-array
                :initial-contents
                (loop
                  for idx below array-size
                  collect (make-instance 'model-array
                                         :arr *audio-preset-ctl-vector*
                                         :a-ref (list idx))))))

;;; (setf (val (aref *audio-preset-ctl-model* (+ 2 (* 16 (player-aref :default))))) 31)

(defparameter *cc-state*
  (make-array '(6 128)
              :element-type 'integer
              :initial-element 0))

(defparameter *cc-fns*
  (make-array '(6 128)
              :element-type 'function
              :initial-element #'identity))

(defun sub-array (main idx &key (size 128))
  (make-array size :displaced-to main :displaced-index-offset (* idx size)))

(defmacro set-cc ((player idx) &body body)
  `(setf (aref *cc-fns* (player-aref ,player) ,idx)
         (lambda (val) ,@body)))

(defun identity-notefn (keynum velo)
  (list keynum velo))

(defparameter *note-states* ;;; stores last note-on keynum for each player.
  (make-array '(16) :element-type 'integer :initial-element 0))
(defparameter *note-fns*
  (make-array '(16) :element-type 'function :initial-element #'identity-notefn))



(declaim (inline last-keynum))
(defun last-keynum (player)
  (aref *note-states* player))

(defun clear-cc-fns ()
  "set all cc-fns to #'identity."
  (do-array (idx *cc-fns*)
    (setf (row-major-aref *cc-fns* idx) #'identity)))

(defun clear-note-fns ()
  (dotimes (n 16)
    (setf (aref *note-fns* n) #'identity)))

;;; (clear-note-fns)

;;; (clear-cc-fns)
|#


;;; (setf *midi-debug* t)


;; (set-fader (find-gui :bs1) 0 29)
;;; (setf *midi-debug* nil)
;;; (start-midi-receive)
#|
(defun set-pad-note-fn-bs-save (player)
  (setf (aref *note-fns* (player-aref player))
        (lambda (keynum velo)
          (declare (ignore velo))
          (cond
            ((<= 44 keynum 51) (bs-state-recall (- keynum 44)))
            ((<= 36 keynum 43) (bs-state-save (- keynum 36)))
            (:else (warn "~&pad num ~a not assigned!" keynum))))))

(defun set-pad-note-fn-bs-trigger (player)
  (setf (aref *note-fns* (player-aref player))
        (lambda (keynum velo)
          (declare (ignore velo))
          (cond
            ((<= 51 keynum 51)
             (cl-boids-gpu::timer-remove-boids
              *boids-per-click* *boids-per-click* :fadetime 0))
            ((<= 36 keynum 50)
             (let* ((ip (interp keynum 36 0 51 1.0))
                    (x (interp (/ (mod ip 0.25) 0.25) 0 0.2 1 1.0))
                    (y (interp (* 0.25 (floor ip 0.25)) 0 0.1 1 1.1)))
               (cl-boids-gpu::timer-add-boids *boids-per-click* 10 :origin `(,x ,y))))
            (:else (warn "~&pad num ~a not assigned!" keynum))))))

;;; (set-pad-note-fn-bs-save :player3)
;;; (set-pad-note-fn-bs-trigger :arturia)

(set-cell (aref *audio-preset-ctl-model* (+ (* 16 (player-aref :default)) 3)) 1) 
(setf *midi-debug* nil)                                      ;
()

*audio-preset-ctl-vector*
*audio-preset-ctl-model*


(loop
  for arg below 16
  with player-offs = (* 16 (player-aref :default))
  for idx = (+ player-offs arg)
  do (set-ref (aref (cuda-gui::param-boxes (find-gui :bs1)) idx)
              (aref *audio-preset-ctl-model* idx)))

|#
