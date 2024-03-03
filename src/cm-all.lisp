;;;
;;; cm-all.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package #:clog)

(defun cm-gui (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Common Music Gui")
  (add-class body "w3-blue-grey"))

(set-on-new-window #'cm-gui :boot-file "/start.html")

(set-on-new-window #'cm-gui :boot-file "/start.html")


(set-on-new-window #'clog-dsp-widgets:levelmeter-full-gui :path "/meters" :boot-file "/start.html")

(in-package #:cm)


(defparameter *mt-out01* nil)

(defmacro make-mt-stream (symbol-name midi-out-stream chan-tuning)
  "Define, open and initialize a microtonal midistream. The name of
the stream and an already initialized midi-port-stream has to be
supplied and gets interned as a parameter."
  `(progn
     (defparameter ,symbol-name
       (new incudine-stream
         :name (string-trim '(#\*) (format nil "~a" ',symbol-name))
         :output ,midi-out-stream))
     (open-io (apply #'init-io ,symbol-name `(:channel-tuning ,,chan-tuning))
              :output ,midi-out-stream)
     (initialize-io ,symbol-name)
     (values ',symbol-name)))

;;; Initialisierung der Mikrotöne:

(defun reinit-midi ()
  (cm::initialize-io *mt-out01*)
  (write-event (new midi-program-change :program 1) *mt-out01* 0)
  (events
   (loop for idx below 8
         collect (new midi :time (* idx 0.05) :keynum (float (+ 60 (* idx 1/4)))))
   *mt-out01*))



#|
(defun get-qsynth-midi-port-name (&optional (direction :input))
  (let ((result (with-output-to-string (str)
                  (progn
                    (uiop:run-program "jack_lsp" :output str)
                    str))))
    (multiple-value-bind (start end reg1 reg2)
        (cl-ppcre:scan (format nil "(a2j:FLUID Synth \\(Qsynth1.*~a.*)\\\n"
                               (if (eq direction :input) "input" "output"))
                       result)
      (declare (ignore end))
      (if start
          (subseq result (aref reg1 0) (aref reg2 0))))))
|#

(defun get-qsynth-midi-port-name (&optional (direction :input))
  (let ((result (uiop:run-program "jack_lsp" :output :string)))
    (multiple-value-bind (start end reg1 reg2)
        (cl-ppcre:scan (format nil "(a2j:FLUID Synth \\(Qsynth1.*~a.*)\\\n"
                               (if (eq direction :input) "input" "output"))
                       result)
      (declare (ignore end))
      (if start
          (subseq result (aref reg1 0) (aref reg2 0))))))

(defun jack-connect-qsynth ()
  (let ((qsynth-port (get-qsynth-midi-port-name)))
    (if qsynth-port
        (uiop:run-program
         (format nil
                 "jack_connect incudine:midi_out-1 \"~a\""
                 qsynth-port)
         :output nil
         :ignore-error-status t))))

(defun restart-qsynth ()
  (let ((result (with-output-to-string (str)
                  (uiop:run-program (format nil "ps aux") :output str))))
    (unless (cl-ppcre:scan "qsynth" result)
      (format t "starting qsynth...")
      (uiop:launch-program (format nil "qsynth") :output nil)
      (sleep 5))
    (jack-connect-qsynth)
    (sleep 1)
    (reinit-midi)))


;; m21
;; trevor
#+swank
;; (progn
;;   (swank:eval-in-emacs
;;    `(load ,(namestring
;;             (asdf:system-relative-pathname :cm-all "elisp/incudine-hush.el"))))
;;   (swank:eval-in-emacs `(slime-repl-eval-string "(cm)")))

#+slynk
(progn
  (slynk:eval-in-emacs
   `(load ,(namestring
            (asdf:system-relative-pathname :cm-all "elisp/incudine-hush-sly.el"))))
      (slynk:eval-in-emacs `(sly-interactive-eval "(cm)")))

(defun start-cm-all (&key (qsynth nil))
  (start-inkscape-osc)
  (rts)
  (unless (cm::rts?) (rts))
  ;;(make-mt-stream *mt-out01* *midi-out1* '(4 0))
  (if qsynth (restart-qsynth))
  ;;(setf *rts-out* *mt-out01*)
  (format t "~&midi initialized!~%")
  (incudine:setup-io)
  (cm))

#|
  ;; (if (member :slynk *features*)
  ;;     (progn
  ;;       (slynk:eval-in-emacs
  ;;        `(load ,(namestring
  ;;                 (asdf:system-relative-pathname :cm-all "elisp/incudine-hush-sly.el"))))
  ;;       (slynk:eval-in-emacs `(sly-interactive-eval "(cm)")))
  ;;     (if (member :swank *features*)
  ;;         (progn
  ;;           (swank:eval-in-emacs
  ;;            `(load ,(namestring
  ;;                     (asdf:system-relative-pathname :cm-all "elisp/incudine-hush.el"))))
  ;;           (swank:eval-in-emacs `(slime-repl-eval-string "(cm)")))))

|#
