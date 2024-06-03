;;;; 1. load the project: 

;;; (ql:quickload "fudi")

;;; 2. start the pd patch.
;;; 3. start inbound and outbound connection:

(in-package :incudine.scratch)

(defvar *fudi-in* nil)
(defvar *fudi-out-udp* nil)
(defvar *fudi-out-tcp* nil)

(setf *fudi-in* (fudi:open :port 3102))
(setf *fudi-out-udp* (fudi:open :port 3000 :protocol :udp :direction :output))
(setf *fudi-out-tcp* (fudi:open :port 3010 :direction :output))

(fudi:close *fudi-out-udp*)

(fudi:stream-open? *fudi-out-tcp*)

(fudi:stream-open? *fudi-out-tcp*)

(eql (fudi::stream-protocol *fudi-out-tcp*) :udp)

(setf *fudi-out-tcp* (fudi:close *fudi-out-tcp*))

(fudi:send *fudi-out-udp* '(1 2 3 4 5))
(fudi:send *fudi-out-tcp* '(1 2 3 "Hallo" 4 5))
(fudi:send *fudi-out-tcp* '("Hallo" "Welt!"))

;;; start server:

(recv-start *fudi-in*)

;;; add a responder

(defparameter *fudi-responder*
  (incudine::make-fudi-responder
   *fudi-in*
   (lambda (msg)
     (format *debug-io* "~a~%" msg))))


(incudine:remove-responder *fudi-responder*)

(fudi:close *fudi-in*)
;;; (fudi:close *fudi-out*)

(setf *fudi-in* (fudi:open :port 3003))

(defparameter *fudi-test* (fudi:open :port 3004))
(defparameter *fudi-responder* nil)
(rt-start)


(fudi-close-default)

;;; *fudi-in*



(fudi:close *fudi-in*)

;;; osc using pd binary:

(progn
  (setf *fudi-in*
        (fudi:open :host "192.168.67.19"
                   :port 3089
                   :direction :input
                   :protocol :udp
                   :element-type 'unsigned-byte))
  (recv-start *fudi-in*)
  (setf *fudi-responder*
        (incudine::make-fudi-responder *fudi-in* (lambda (msg) (format t "~a~%" msg)))))

(progn
  (incudine:remove-responder fudi-responder)
  (recv-stop *fudi-in*)
  (fudi:close *fudi-in*))

;;;; OSC with incudine built-in:





(defparameter *oscin* nil)






(progn
  (setf *oscin* (incudine.osc:open :host "192.168.67.19" :port 3089 :direction :input))
  (recv-start *oscin*)
  (make-osc-responder *oscin* "/xy" "ff"
                      (lambda (a b)
                        (msg warn "xy: ~D ~D" a b)))
  (make-osc-responder *oscin* "/obsttype" "f"
                      (lambda (type)
                        (msg warn "type: ~D" type)))
  (make-osc-responder *oscin* "/obstactive" "f"
                      (lambda (obstactive)
                        (msg warn "active: ~D" obstactive)))
  (make-osc-responder *oscin* "/obstvolume" "f"
                      (lambda (amp)
                        (msg warn "amp: ~D" amp))))

#|
(progn
  (recv-stop *oscin*)
  (remove-all-responders *oscin*)
  (incudine.osc:close *oscin*))
|#

;;; incudine::*responders*
