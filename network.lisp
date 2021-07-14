;;; 
;;; network.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-poolplayer)

(defparameter *fudi-out* nil)

(defun init-netsend ()
  (if *fudi-out* (fudi:close *fudi-out*))
  (setf *fudi-out*
        (fudi:open :host *remote-ip* :port *port* :direction :output)))

;;; (init-netsend)

(defun send-to-remote (&rest params)
  "send params to remote player. As the remote player starts with
channel 9, decrease the out channels by 9 and only send if both outs
are positive. Before sending replace the :buffer param by its index in
*buffers*. This requires that the buffer-ids of the buffers in local
player and remote-player have to match!"
  (let ((out1 (decf (getf params :out1) 9))
        (out2 (decf (getf params :out2) 9)))
;;;    (format t "~&~a, ~a" params (max out1 out2))
    (if (>= (max out1 out2) 0)
        (let ((buf (getf params :buffer)))
          (setf (getf params :buffer) (buf-idx buf))
          (if (fudi:stream-open? *fudi-out*)
              (progn
;;;                (format t "~&sending: ~a" params)
                (fudi:send *fudi-out* (list (format nil "~S" params))))
              (warn "stream not open: ~a" *fudi-out*))))))



;;; (fudi:stream-open? *fudi-out*)

;;; (fudi:close *fudi-out*)
#|

;;; (init-netsend)

(send-to-remote
 :buffer (aref *buffers* 4)
:amp -6 :transp -11.047948 :start 0 :end 0 :stretch 1.0 :wwidth 123
:attack 0 :release 0.01 :pan 0.5 :out1 1 :out2 12)

(fudi:close *fudi-out*)
|#


#|
(fudi:send *fudi-out* (list (format nil "~S" '(:buffer 4
 :amp -6 :transp -11.047948 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01
 :pan 0.5 :out1 9 :out2 10))))

(format nil "~S" '(:buffer 1
 :amp -6 :transp -11.047948 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01
 :pan 0.5 :out1 10 :out2 11))

(fudi:send *fudi-out* (list "(hallo peng)"))


(incf)                                      ;
(fudi:send *fudi-out* (list (format nil "~S" )))

|#
