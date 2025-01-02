;;;; -*- Mode: lisp -*-
;;;
;;; cuda-usocket-osc.lisp
;;;
;;; thin implementation of osc for incudine based on usocket to
;;; circumvent issues with OSX.
;;;
;;;
;;; Copyright (c) 2024 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package #:cuda-usocket-osc)

;;; private global defaults:

(defvar *server* nil)
(defvar *out-socket* nil)
(defvar *out-stream* *standard-output*)
(defvar *host* "127.0.0.1")
(defvar *in-port* 3005)
(defvar *out-port* 3006)

(defstruct (cu-osc-stream (:constructor %make-cu-osc-stream)(:copier nil))
  (host *host* :type simple-string)
  (port *in-port* :type (unsigned-byte 16))
  (protocol :tcp :type (member :udp :tcp))
  (elt-type 'character :type (member character unsigned-byte))
  (direction :input :type (member :input :output))
  (socket nil))

(defstruct (input-cu-osc-stream (:include cu-osc-stream))
  (server-running? nil)
  (server-socket nil)
  (receiver nil)
  (id (format nil "~s" (gensym)) :type simple-string))

(defstruct (output-cu-osc-stream (:include cu-osc-stream))
  (send-fn (lambda (&rest args) (declare (ignore args))) :type function))

(defmethod print-object ((obj cu-osc-stream) stream)
  (format stream "#<OSC:~A-STREAM ~S ~S ~D>"
          (cu-osc-stream-direction obj) (cu-osc-stream-protocol obj)
          (cu-osc-stream-host obj) (cu-osc-stream-port obj)))

(defun %open (stream &rest args)
  (declare (type cu-osc-stream stream))
  (if (input-stream-p stream)
      (start-socket-server stream)
      (let ((socket (usocket:socket-connect
                     (cu-osc-stream-host stream) (cu-osc-stream-port stream)
                     :element-type (getf args :element-type)
                     :protocol (case (cu-osc-stream-protocol stream)
                                 (:udp :datagram)
                                 (t :stream)))))
        (setf (cu-osc-stream-socket stream) socket)
        (setf (output-cu-osc-stream-send-fn stream)
              (case (cu-osc-stream-protocol stream)
                (:tcp (let ((s (usocket:socket-stream socket)))
                        (lambda (&rest args)
                          (format s "~{~a~^ ~};~%" args)
                          (force-output s))))
                (t (lambda (vec)
                     (usocket:socket-send socket vec (length vec))))))))
  stream)

(defun make-adjustable-binary-vector ()
  "create an adjustable vector to store incoming osc
messages."
  (make-array 1
              :fill-pointer 1
              :adjustable t
              :element-type 'unsigned-byte))

(defun cuda-osc-read-binary-message (s &optional
                            (input-stream *standard-input*)
                            recursive-p)
  "Read bytes from INPUT-STREAM into a vector until no characters left to read.
 return the parsed vector."
  (declare (ignore recursive-p))
  (setf (fill-pointer s) 1)
  (loop
     for c = (read-byte input-stream nil nil)
     while c
     do (vector-push-extend c s)
     finally (return s)))

(defun default-udp-binary-socket-handler (socket-data osc-stream)
  (declare (type vector socket-data) (type input-cu-osc-stream osc-stream))
  (let ((receiver (input-cu-osc-stream-receiver osc-stream)))
    (if (and receiver (incudine::receiver-status receiver))
        (handler-case
            (dolist (fn (incudine::receiver-functions receiver))
              (funcall (the function fn) socket-data))
          (condition (c) (incudine::nrt-msg error "~A" c))))))

(defun default-tcp-binary-socket-handler (socket-stream osc-stream)
  (declare (type cl:stream socket-stream) (type input-cu-osc-stream osc-stream))
  (loop
    for msg = (read-byte socket-stream nil nil)
    while (input-cu-osc-stream-server-running? osc-stream)
    if msg do (let ((receiver (input-cu-osc-stream-receiver osc-stream)))
                (if (incudine::receiver-status receiver)
                    (handler-case
                        (dolist (fn (incudine::receiver-functions receiver))
                          (funcall (the function fn) msg))
                      (condition (c) (incudine::nrt-msg error "~A" c)))))))

(defun destroy-named-thread (name)
  (loop
     for thread in (bt:all-threads)
     do (if (equal (thread-name thread) name)
            (return (bt:destroy-thread thread)))))

(defun stop-socket-server (stream)
  (declare (type cu-osc-stream stream))
;;  (break "stream: ~a id: ~a" stream (input-cu-osc-stream-id stream))
  (destroy-named-thread (input-cu-osc-stream-id stream))
  (setf (input-cu-osc-stream-server-running? stream) nil))

(defun get-socket-handler (stream)
  (case (cu-osc-stream-protocol stream)
    (:tcp #'default-tcp-binary-socket-handler)
    (:udp #'default-udp-binary-socket-handler)))

(defmethod input-stream-p ((stream cu-osc-stream))
  (eql (cu-osc-stream-direction stream) :input))

(defun start-socket-server (stream)
  (if (input-stream-p stream)
      (multiple-value-bind (thread server-socket)
          (usocket:socket-server
           (cu-osc-stream-host stream)
           (cu-osc-stream-port stream)
           (get-socket-handler stream)
           (list stream) 
           :in-new-thread t
           :name (input-cu-osc-stream-id stream)
           :protocol (case (cu-osc-stream-protocol stream)
                       (:udp :datagram)
                       (:tcp :stream)
                       (t (error "protocol: ~s not supported!"
                                 (cu-osc-stream-protocol stream))))
           :reuse-address t
           :multi-threading t
           :element-type (cu-osc-stream-elt-type stream))
        (if thread (progn
                     (setf (input-cu-osc-stream-server-running? stream) t)
                     (setf (input-cu-osc-stream-server-socket stream) server-socket)))
        thread)))

(defun start-osc-recv (receiver)
  (let ((stream (incudine::receiver-stream receiver)))
    (if (or (input-cu-osc-stream-server-running? stream)
            (start-socket-server stream))
        (progn
          (setf (input-cu-osc-stream-receiver stream) receiver)
          (setf (incudine::receiver-status receiver) t)))
    receiver))

(defun stop-osc-recv (receiver)
  (incudine::compare-and-swap (incudine::receiver-status receiver) t nil)
  receiver)

(defun remove-receiver (stream)
  (declare (type input-cu-osc-stream stream))
  (if (and stream (input-cu-osc-stream-receiver stream))
      (progn
        (stop-osc-recv (input-cu-osc-stream-receiver stream))
        (incudine::remove-receiver stream))))

(defun open-cu-osc (&key (host *host*) (port *in-port*) (direction :input)
               (element-type 'character) (protocol :udp))
  (declare (type (member :input :output) direction)
           (type (member :udp :tcp) protocol)
           (type (member character unsigned-byte) element-type)
           (type simple-string host))
  (let* ((obj (funcall (if (eq direction :input)
                           #'make-input-cu-osc-stream
                           #'make-output-cu-osc-stream)
                       :host host :port port :protocol protocol
                       :direction direction
                       :elt-type element-type)))
    (handler-case
        (%open obj)
      (error (c)
        (close obj)
        (incudine.util:msg error "OSC:OPEN ~A (~A)" c
                           (incudine.external:errno-to-string))))))

(defun close-cu-osc (stream)
  (if stream
      (progn
        (if (input-stream-p stream)
            (progn
              (remove-receiver stream)
              (stop-socket-server stream)
              (usocket:socket-close (input-cu-osc-stream-server-socket stream))))
        (if (cu-osc-stream-socket stream)
            (usocket:socket-close (cu-osc-stream-socket stream))))))

(defun out-stream-open? (s)
  (and s
       (output-stream-p s)
       (open-stream-p
        (slot-value (cu-osc::cu-osc-stream-socket s) 'usocket::stream))))

(defun send (stream msg)
  (cond ((and (eql (cu-osc::cu-osc-stream-protocol stream) :tcp)
              (out-stream-open? stream))
         (funcall (output-cu-osc-stream-send-fn stream) msg))
        ((eql (cu-osc::cu-osc-stream-protocol stream) :udp)
         (funcall (output-cu-osc-stream-send-fn stream) msg))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun match-address (message address)
    "Only return T if the the first (length address) elems of message match
address."
    (loop
      for charcode1 across address
      for charcode2 across message
      while (= charcode1 charcode2)
      finally (return (= charcode1 charcode2)))))

;;; (match-address #(1 2 3 4) #(1 2 3))

(defun decode-message (message)
  "reduces an osc message to an (address . data) pair. .."
  (declare (type (vector *) message))
  (let* ((x (position (char-code #\,) message))
         (x2 (and x (+ x (position 0 (subseq message x))))))
    (break "~a ~a" x x2)
    (if (eq x nil)
        (format t "message contains no data.. ")
        (list (osc::decode-address (subseq message 0 x))
              (osc::decode-address (subseq message (1+ x) x2))
              (osc::decode-taged-data (subseq message x))))))


(defun encode-typetag (string)
  "creates a typetag string suitable for the given data.
  valid typetags according to the osc spec are ,i ,f ,s and ,b
  non-std extensions include ,{h|t|d|S|c|r|m|T|F|N|I|[|]}
                             see the spec for more details. ..

  NOTE: currently handles the following tags
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string
   b => #(98)  => blob
   h => #(104) => int64
  and considers non int/float/string data to be a blob."

  (let ((lump (make-array 0 :adjustable t
                          :fill-pointer t)))
    (macrolet ((write-to-vector (char)
                 `(vector-push-extend
                   (char-code ,char) lump)))
      (write-to-vector #\,)
      (map nil
           (lambda (char) 
             (vector-push-extend (char-code char) lump))
           string))
    (osc::cat lump
              (osc::pad (osc::padding-length (length lump))))))

(defun encode-args (args)
  "encodes args in a format suitable for an OSC message"
  (let ((lump (make-array 0 :adjustable t :fill-pointer t)))
    (macrolet ((enc (f)
                 `(setf lump (osc::cat lump (,f x)))))
      (dolist (x args)
        (typecase x
          (integer (if (>= x 4294967296)
                       (enc osc::encode-int64)
                       (enc osc::encode-int32)))
          (float (enc osc::encode-float32))
          (simple-string (enc osc::encode-string))
          (t (enc osc::encode-blob))))
      lump)))

;;; #+cuda-usocket-osc

(defmethod incudine::valid-input-stream-p ((obj input-cu-osc-stream)) t)

(defmethod incudine::valid-input-stream-p ((obj output-cu-osc-stream)) nil)

(defmethod incudine::recv-start ((stream input-cu-osc-stream)
                                 &key (priority incudine::*receiver-default-priority*))
  (incudine::add-receiver stream (or (incudine:receiver stream)
                                     (incudine::make-receiver stream))
                          #'start-osc-recv priority))

(defmethod incudine::recv-stop ((stream input-cu-osc-stream))
  (stop-osc-recv (input-cu-osc-stream-receiver stream)))
#+darwin
(pushnew :cuda-usocket-osc *features*)

(if (member :cuda-usocket-osc *features*)
    (progn
      (format t "~&shadowing incudine.osc functions with cuda-usocket-osc~%")
      (setf (fdefinition 'incudine.osc::open) #'open-cu-osc)
      (setf (fdefinition 'incudine.osc::close) #'close-cu-osc)
      (unintern 'incudine.osc:message)
      (define-compiler-macro incudine.osc:message (stream address types &rest values)
        `(cu-osc:send ,stream
                      (concatenate '(vector (unsigned-byte 8))
                                   (osc::encode-address ,address)
                                   (encode-typetag ,types)
                                   (osc::encode-args (list ,@(loop for value in values
                                                                   collect value)))))))
    (progn
      (format t "~&not shadowing incudine.osc functions~%")))

(defun incudine.osc:message (stream address types &rest values)
  (cu-osc:send stream
               (concatenate
                '(vector (unsigned-byte 8))
                (osc::encode-address address)
                (encode-typetag types)
                (encode-args values))))

(in-package :incudine)

(when (member :cuda-usocket-osc *features*)
  (shadowing-import '(make-osc-responder remove-osc-responder) 'cuda-usocket-osc)
  (defmacro make-osc-responder (stream address types function)
    "Create and return a responder for a OSC:INPUT-STREAM that responds
to an OSC message with ADDRESS and TYPES.

Note: the receiver reads the OSC messages from an OSC bundle too,
therefore an OSC bundle doesn't require a specific responder.
See OSC:RECEIVE and OSC:MESSAGE-TIME for details.

FUNCTION is added to the list of receiver-functions for STREAM.

The function takes the OSC values as arguments.

Example:

    (make-osc-responder *oscin* \"/osc/test\" \"iii\"
                        (lambda (a b c)
                          (msg warn \"~D ~D ~D\" a b c)))"
    (let ((fnc function)
          (address-vector (map 'vector #'char-code address))
          (num-types (length types)))
      (with-gensyms (message)
        `(incudine:make-responder
          ,stream
          (lambda (,message)
            (incudine.util:msg :debug "osc-in: ~a" ,message)
            (let* ((x1 (position (char-code #\,) ,message))
                   (x2 (+ (or x1 0) (position 0 (subseq ,message (1+ x1))))))
              (if (and (cuda-usocket-osc::match-address ,message ,address-vector)
                       (= ,num-types (- x2 x1)))
                  (apply ,fnc (osc::decode-taged-data (subseq ,message x1)))))
            (values))))))
  (defun remove-osc-responder (resp)
    (if resp (incudine:remove-responder resp))))

;;; (let ((recv (receiver stream))))



#|
(defun parse-osc-string (str)
  (read-from-string
   (format nil "(~a)"
(map 'string #'code-char str))))

;;; (parse-osc-string #(49 50 32 49 52))

(defun pad (n)
  "make a sequence of the required number of #\Nul characters"
  (declare (type fixnum n))
  (make-array n :initial-element 0 :fill-pointer n))

(defun cat (&rest catatac)
  (apply #'concatenate '(vector (unsigned-byte 8)) catatac))

(defun padding-length (s)
  "returns the length of padding required for a given length of string"
  (declare (type fixnum s))
  (- 4 (mod s 4)))

(defun string-padding (string)
  "returns the padding required for a given osc string"
  (declare (type simple-string string))
  (pad (padding-length (length string))))

(defun encode-address (address)
  (cat (map 'vector #'char-code address)
       (string-padding address)))

(defun encode-string (string)
  "encodes a string as a vector of character-codes, padded to 4 byte boundary"
  (cat (map 'vector #'char-code string)
       (string-padding string)))
|#

