(unless (find-package :clamps)
  (progn
    (ql:quickload :clamps)
    (clamps)))

(defpackage #:clog-widgets-example
  (:shadowing-import-from #:clog
                          #:font-style #:transform #:name #:display #:width #:height #:rotate #:inverse #:opacity)
  (:use #:cl #:clamps #:clog))

;; then select everything from here down to the end of the file and
;; complire/evaluate it.

(in-package :clog-widgets-example)


(defun create-o-div (parent bindings &key width height css)
  (declare (ignorable width height))
  (let* ((element (create-div
                   parent
                   (format nil "<o-numbox min=\"~a\" max=\"~a\" value=\"~a\" precision=\"~a\" ~@[~a~]>"
                           min max (get-val var) precision
                           (format-style css))))
         ) ;;; the get-val automagically registers the ref
    (dolist (binding bindings) (push element (b-elist binding))
      (setf (attribute element (b-attr binding)) (get-val (b-ref binding)))) ;;; register the browser page's html elem for value updates.
    (set-on-data ;;; react to changes in the browser page
     element
     (lambda (obj data)
       (declare (ignore obj))
       ;; (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
       ;;                     (or (if (gethash "close" data) "close")
       ;;                         (gethash attr data))))
       (if (gethash "close" data)
           (progn
;;;             (format t "closing numbox~%")
             (dolist (binding bindings) (setf (b-elist binding) (remove element (b-elist binding))))) ;;; cleanup: unregister elem.
           (let ((*refs-seen* (list (list element attr))))
;;;             (format t "~&numbox recv value: ~a, ~a~%" (float (gethash attr data) 1.0) *refs-seen*)
             (%set-val var (float (gethash attr data) 1.0))
             ))))
    element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rms->db (amp)
  (if (zerop amp)
      -100
      (* 20 (log amp 10))))

(defun db->rms (db)
  (expt 10 (/ db 20)))

(defun clip (val minvalue maxvalue)
  (min maxvalue (max minvalue val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; define some variables

(progn
  (defparameter x-bang nil)
  (defparameter x nil)
  (defparameter x-db nil)
  (defparameter radio nil)
  (defparameter mslider nil))

;;;(trigger x-bang)

;;; (set-val x 0.4)

(progn
  (clear-bindings)
  (setf x (make-ref 0.5))
  (setf x-bang (make-bang (lambda () (set-val x 0))))
  (setf x-db
        (make-computed
         (lambda () (clip (round (rms->db (get-val x))) -40 0))
         (lambda (val) (%set-val x (clip (float (if (<= val -40) 0 (db->rms val))) 0 1)))))
  (setf radio
        (make-computed
         (lambda () (round (/ (+ 40 (get-val x-db)) 40/7)))
         (lambda (val) (%set-val x-db (- (float (* val 40/7)) 40)))))
  (setf mslider (make-array 8 :initial-contents (loop repeat 8 collect (make-ref 0))))
  (setf (aref mslider 0) x)
  nil)

;;; Define our CLOG application

#|

(defparameter *meineaktion* (watch (lambda () (format t "~,2f~%" (get-val x)))))
(funcall *meineaktion*)

|#

(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Gui Test")
  (let ((collection (create-collection body "1/2")))
    (create-o-vumeter collection (bind-refs-to-attrs x-db "db-value") :mapping :pd)
    (create-o-numbox collection (bind-refs-to-attrs x "value") :min 0 :max 1 :precision 2
                     :css '(:height 1em))
    (create-o-knob collection (bind-refs-to-attrs x "value") :min 0 :max 1 :step 0.01)
    (create-o-knob collection (bind-refs-to-attrs x "value") :min 0 :max 1 :step 0.01)
    (create-o-knob collection (bind-refs-to-attrs x-db "value") :min -40 :max 0 :step 1 :unit "dB" :precision 0)
    (create-o-bang collection (bind-refs-to-attrs x-bang "bang")
                   :css '(:height 1em))
    (create-o-toggle collection (bind-refs-to-attrs x "value")
                     :css '(:height 1em))
    (create-o-radio collection (bind-refs-to-attrs radio "value")
                    :direction :up
                    :background '(("#444" "#888") ("orange"))
                    :labels (list (loop for num below 8 collect num)))
    (create-o-slider collection (bind-refs-to-attrs x "value") :background "transparent")
    (create-o-multislider collection (bind-refs-to-attrs mslider "value")
                          :colors '("#8f8" "#f88" "#44f") :background "transparent")))

;;; We don't want to restart the server everytime when the new-window
;;; fun is canged thats why this proxy gets defined

(defun on-new-window (body)
  (new-window body))

(set-on-new-window #'on-new-window :path "/example")

