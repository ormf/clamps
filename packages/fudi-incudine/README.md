# OVERVIEW

fudi is a common lisp package which establishes a bidirectional
connection to pd for incudine using the FUDI protocol
(see: https://en.wikipedia.org/wiki/FUDI).

Written by Orm Finnendahl 2016, public domain, no warranties
whatsoever.

# PREREQUISITES

Lisp with quicklisp installed. Pure Data installed.

# INSTALL

1. link the folder "fudi" into "~/quicklisp/local-projects/".


# USAGE

;;;; 1. load the project: 

;;; (ql:quickload "fudi")

;;; 2. start the pd patch "

;;; 3. start inbound and outbound connection:

(in-package :incudine.scratch)

(defvar *fudi-in* (fudi:open :port 3015))
(defvar *fudi-out* (fudi:open :port 3008 :protocol :udp :direction :output))
(defvar *fudi-out2* (fudi:open :port 3012 :direction :output))

(fudi:send *fudi-out* '(1 2 3 4 5))
(fudi:send *fudi-out2* '(1 2 3 "Hallo" 4 5))
(fudi:send *fudi-out2* '("Hallo" "Welt!"))

(recv-start *fudi-in*)

(defvar *fudi-responder*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (format *debug-io* "~a~%" msg))))

(incudine:remove-responder *fudi-responder*)

(defvar *fudi-responder2*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (fudi:send *fudi-out* msg))))

(incudine:remove-responder *fudi-responder2*)

(setf *fudi-responder2*
      (incudine::make-fudi-responder
       *fudi-in*
       (lambda (msg)
         (fudi:send *fudi-out* msg))))

(incudine:recv-stop *fudi-in*)
(incudine:recv-start *fudi-in*)


(incudine:remove-responder *fudi-responder2*)

(fudi:close *fudi-in*)
(fudi:close *fudi-out*)

(incudine:receiver *fudi-in*)


#|(maphash #'(lambda (key val)
             (print (list key val)))
         *receiver-hash*)
|#


Usage is explained in the file "fudi-example.lisp".
