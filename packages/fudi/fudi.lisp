;;;; -*- Mode: lisp -*-
;;;
;;; fudi.lisp
;;;
;;; Copyright (c) 2016-19 Orm Finnendahl
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

(in-package #:fudi)
(defvar *server* nil)
(defvar *out-socket* nil)
(defvar *out-stream* *standard-output*)
(defvar *host* "127.0.0.1")
(defvar *in-port* 3005)
(defvar *out-port* 3006)

(defstruct (stream (:constructor %make-stream)(:copier nil))
  (host *host* :type simple-string)
  (port *in-port* :type (unsigned-byte 16))
  (protocol :tcp :type (member :udp :tcp))
  (elt-type 'character :type (member character unsigned-byte))
  (direction :input :type (member :input :output))
  (socket nil))

(defstruct (input-stream (:include stream))
  (server-running? nil)
  (server-socket nil)
  (receiver nil)
  (id (format nil "~s" (gensym)) :type simple-string))

(defstruct (output-stream (:include stream))
  (send-fn (lambda (&rest args) (declare (ignore args))) :type function))

(defmethod print-object ((obj stream) stream)
  (format stream "#<FUDI:~A-STREAM ~S ~S ~D>"
          (stream-direction obj) (stream-protocol obj)
          (stream-host obj) (stream-port obj)))

(defun %open (stream &rest args)
  (declare (type stream stream))
  (cond ((input-stream-p stream) nil)
        (T (let ((socket (usocket:socket-connect
                          (stream-host stream) (stream-port stream)
                          :element-type (getf args :element-type)
                          :protocol (case (stream-protocol stream)
                                      (:udp :datagram)
                                       (t :stream)))))
             (setf (stream-socket stream) socket)
             (setf (output-stream-send-fn stream)
                   (case (stream-protocol stream)
                     (:tcp (let ((s (usocket:socket-stream socket)))
                             (lambda (&rest args)
                               (format s "~{~a~^ ~};~%" args)
                               (force-output s))))
                     (t (lambda (&rest args)
                          (let ((str (format nil "~{~a~^ ~};~%" args)))
                            (usocket:socket-send socket str (length str))))))))))
  stream)

#|

;;; queued Server using sb-concurrency. The code hassn't yet been adopted to
;;; work with incudine.

(require :sb-concurrency)

(defparameter *local-queue* nil)

(defun default-queued-socket-handler (stream)
  (declare (type cl:stream stream))
  (loop
     for received = (read-delimited-list #\; stream)
     while received
     do (if *local-queue*
            (sb-concurrency:send-message *local-queue* received)
            (progn
	      (format *debug-io* "no event queue!~%")
	      (force-output *debug-io*)))))


(defun start-queued-socket-server (queue &key (outhost "localhost") (outport 3000) 
                              (inhost "localhost") (inport 3001)
                              (protocol :stream))
  (declare (ignore protocol))
  (stop-socket-server nil)
  (if *out-socket* (usocket:socket-close *out-socket*))
  (setf *out-socket* (usocket:socket-connect outhost outport))
  (setf *out-stream* (usocket:socket-stream *out-socket*))
  (setf *local-queue* queue)
  (setf *server*
        (usocket:socket-server inhost inport #'default-queued-socket-handler nil 
                               :in-new-thread t
                               :name "Fudi-Responder"
;                               :protocol protocol
                               :reuseaddress t
                               :multi-threading t)))

|#

(defun make-adjustable-string ()
  "create an adjustable string to store incoming fudi
messages. Intitalize with an open parenthesis."
  (make-array 1
              :fill-pointer 1
              :adjustable t
              :initial-contents "("
              :element-type 'character))

(defun make-adjustable-binary-vector ()
  "create an adjustable string to store incoming fudi
messages. Intitalize with an open parenthesis."
  (make-array 1
              :fill-pointer 1
              :adjustable t
              :element-type 'unsigned-byte))

(defun fudi-read-ascii-message (s &optional
                            (input-stream *standard-input*)
                            recursive-p)
  "Read chars from INPUT-STREAM into a string until the next character
   is #\; (or EOF) and return the parsed string as list."
  (declare (ignore recursive-p))
  (setf (fill-pointer s) 1)
  (loop
     for c = (read-char input-stream nil nil)
     until (or (not c) (char= c #\;))
     do (vector-push-extend c s)
     finally (return (progn
                       (vector-push-extend #\) s) ;; add close parenthesis
                       (read-from-string s)))))

(defun fudi-read-binary-message (s &optional
                            (input-stream *standard-input*)
                            recursive-p)
  "Read bytes from INPUT-STREAM into a vector until no characters left to read.
 return the parsed vector."
  (declare (ignore recursive-p))
  (setf (fill-pointer s) 1)
  (loop
     for c = (read-byte input-stream nil nil)
     while c
     do (progn
          (format t "~a, " c)
          (vector-push-extend c s))
     finally (return s)))

(defun parse-fudi-string (str)
  (read-from-string
   (format nil "(~a)"
           (map 'string #'code-char str))))

;;; (parse-fudi-string #(49 50 32 49 52))



(defun default-udp-ascii-socket-handler (socket-data fudi-stream)
  (declare (type vector socket-data) (type input-stream fudi-stream))
  (let ((receiver (input-stream-receiver fudi-stream)))
    (if (incudine::receiver-status receiver)
        (handler-case
            (dolist (fn (incudine::receiver-functions receiver))
              (funcall (the function fn) (parse-fudi-string socket-data)))
          (condition (c) (incudine::nrt-msg error "~A" c))))))

(defun default-udp-binary-socket-handler (socket-data fudi-stream)
  (declare (type vector socket-data) (type input-stream fudi-stream))
  (let ((receiver (input-stream-receiver fudi-stream)))
    (if (incudine::receiver-status receiver)
        (handler-case
            (dolist (fn (incudine::receiver-functions receiver))
              (funcall (the function fn) socket-data))
          (condition (c) (incudine::nrt-msg error "~A" c))))))

(defun default-tcp-ascii-socket-handler (socket-stream fudi-stream)
  (declare (type cl:stream socket-stream) (type input-stream fudi-stream))
  (let ((s (make-adjustable-string)))
    (loop
      for msg = (fudi-read-ascii-message s socket-stream)
      while msg
      do (let ((receiver (input-stream-receiver fudi-stream)))
           (if (incudine::receiver-status receiver)
               (handler-case
                   (dolist (fn (incudine::receiver-functions receiver))
                     (funcall (the function fn) msg))
                 (condition (c) (incudine::nrt-msg error "~A" c))))))))

(defun default-tcp-binary-socket-handler (socket-stream fudi-stream)
  (declare (type cl:stream socket-stream) (type input-stream fudi-stream))
  (loop
    for msg = (read-byte socket-stream nil nil)
    while (input-stream-server-running? fudi-stream)
    if msg do (let ((receiver (input-stream-receiver fudi-stream)))
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
  (declare (type stream stream))
;;  (break "stream: ~a id: ~a" stream (input-stream-id stream))
  (destroy-named-thread (input-stream-id stream))
  (setf (input-stream-server-running? stream) nil))

(defun get-socket-handler (stream)
  (case (stream-protocol stream)
    (:tcp (case (stream-elt-type stream)
            (character     #'default-tcp-ascii-socket-handler)
            (unsigned-byte #'default-tcp-binary-socket-handler)))
    (:udp (case (stream-elt-type stream)
            (character     #'default-udp-ascii-socket-handler)
            (unsigned-byte #'default-udp-binary-socket-handler)))))

(defun start-socket-server (stream)
  (if (input-stream-p stream)
      (multiple-value-bind (thread server-socket)
          (usocket:socket-server
           (stream-host stream)
           (stream-port stream)
           (get-socket-handler stream)
           (list stream) 
           :in-new-thread t
           :name (input-stream-id stream)
           :protocol (case (stream-protocol stream)
                       (:udp :datagram)
                       (:tcp :stream)
                       (t (error "protocol: ~s not supported!"
                                 (stream-protocol stream))))
           :reuse-address t
           :multi-threading t
           :element-type (stream-elt-type stream))
        (usocket:socket-close server-socket)
        (if thread (progn
                     (setf (input-stream-server-running? stream) t)
                     (setf (input-stream-server-socket stream) server-socket)))
        thread)))

(defun start-fudi-recv (receiver)
  (let ((stream (incudine::receiver-stream receiver)))
    (if (or (input-stream-server-running? stream)
            (start-socket-server stream))
        (progn
          (setf (input-stream-receiver stream) receiver)
          (setf (incudine::receiver-status receiver) t)))
    receiver))

(defun stop-fudi-recv (receiver)
  (incudine::compare-and-swap (incudine::receiver-status receiver) t nil)
  receiver)

(defun remove-receiver (stream)
  (declare (type input-stream stream))
  (if (and stream (input-stream-receiver stream))
      (progn
        (stop-fudi-recv (input-stream-receiver stream))
        (incudine::remove-receiver stream))))

(defun open (&key (host *host*) (port *in-port*) (direction :input)
               (element-type 'character) (protocol :tcp))
  (declare (type (member :input :output) direction)
           (type (member :udp :tcp) protocol)
           (type (member character unsigned-byte) element-type)
           (type simple-string host))
  (let* ((obj (funcall (if (eq direction :input)
                           #'make-input-stream
                           #'make-output-stream)
                       :host host :port port :protocol protocol
                       :direction direction
                       :elt-type element-type)))
    (handler-case
        (%open obj)
      (error (c)
        (close obj)
        (incudine.util:msg error "FUDI:OPEN ~A (~A)" c
                           (incudine.external:errno-to-string))))))

(defun close (stream)
  (if stream
      (progn
        (if (input-stream-p stream)
            (progn
;;              (break "closing input: ~a" stream)
              (remove-receiver stream)
              (stop-socket-server stream)
              (usocket:socket-close (input-stream-server-socket stream))))
        (if (stream-socket stream)
            (usocket:socket-close (stream-socket stream))))))

(defun stream-open? (s)
  (and s (open-stream-p
          (slot-value (fudi::stream-socket s) 'usocket::stream))))

(defun send (stream msg)
  (if (and (output-stream-p stream)
           (eql (fudi::stream-protocol stream) :tcp)
           (stream-open? stream))
      (apply (output-stream-send-fn stream) msg)))

;;;(assert)



(defmethod incudine::valid-input-stream-p ((obj input-stream)) t)
(defmethod incudine::valid-input-stream-p ((obj output-stream)) nil)

(defmethod incudine::recv-start ((stream input-stream)
                                 &key (priority incudine::*receiver-default-priority*))
  (incudine::add-receiver stream (or (incudine:receiver stream)
                                     (incudine::make-receiver stream))
                          #'start-fudi-recv priority))

(defmethod incudine::recv-stop ((stream input-stream))
  (stop-fudi-recv (input-stream-receiver stream)))

;;; (let ((recv (receiver stream))))

(defun incudine::make-fudi-responder (stream function)
  (declare (type function function))
  (incudine:make-responder stream function))

(defun incudine::remove-fudi-responder (resp)
  (if resp (incudine:remove-responder resp)))
