;;;  define the system and its dependencies:



;;; load depedencies

(ql:quickload :clog-dsp-widgets)

;;; define the package and tell it to use all dependencies

(defpackage #:clog-widgets-example
  (:use #:cl #:clog #:cl-refs #:clog-dsp-widgets))

;; then select everything from here down to the end of the file and
;; complire/evaluate it.

(in-package :clog-widgets-example)

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

(setf *debug* nil)

(progn
  (defparameter x-bang nil)
  (defparameter x nil)
  (defparameter x-db nil)
  (defparameter radio nil)
  (defparameter mslider nil))

;;;(trigger x-bang)

(set-val x 0.4)

(defparameter niklas (watch (lambda () (format t "x geändert: ~a~%" (get-val x)))))

(funcall niklas)

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
  (setf (title (html-document body)) "SVG Test")
  (create-o-svg body (bind-refs-to-attrs x "cursor-pos" y "shift") :svg "/html-display.svg")
  (create-o-knob body (bind-refs-to-attrs x "value") 0 1 0.01 :precision 2)
    )

(defparameter test (reduce #'bind-ref-to-attr '((x "cursor-pos") (y "shift"))))

(defparameter test (reduce (lambda (prev x) (cons (apply #'bind-ref-to-attr x) prev))
                           `((,x "cursor-pos") (,y "shift"))))




(defparameter y (make-ref 0))

(setf test (apply #'bind-ref-to-attr (list x "value")))

(funcall test)
(set-val x 0.4)


;;; We don't want to restart the server everytime when the new-window
;;; fun is canged thats why this proxy gets defined
(defun on-new-window (body)
  (new-window body))

;; Initialize the CLOG system with a boot file which contains the
;; static js files. For customized uses copy the "www" subdirectory of
;; the repository to your local project and adjust :static-root
;; accordingly
(defun start ()
  (clear-bindings) ;;; start from scratch
  (initialize #'on-new-window
              :port 8080
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

(start)
