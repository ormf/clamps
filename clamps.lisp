;;; 
;;; clamps.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2024 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defun clamps-gui (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Clamps Gui")
  (add-class body "w3-blue-grey"))

(defun clog-dsp-widgets::on-new-window (body)
  "On-new-window handler."
  (funcall #'clamps-gui body))

(set-on-new-window #'clamps-gui :boot-file "/start.html")
(set-on-new-window #'clog-dsp-widgets::meters-window :path "/meters" :boot-file "/start.html")

(in-package #:clamps)

(defparameter *svg-dir* nil)

(defun ensure-directory (dir)
  (if (stringp dir) (format nil "~A/" dir) dir))

(defvar *clamps-gui-root* nil)

(defun clamps-gui-root ()
  *clamps-gui-root*)

(defun svg-gui-path (str)
  (namestring (merge-pathnames (format nil "www/svg/~a" str) (clamps-gui-root))))

(defun clamps-restart-gui (directory &key (start-gui t))
  (let* ((dir (pathname (ensure-directory directory)))
         (svg-dir-path (format nil "~Awww/svg/" (namestring dir))))
    (format t "(re)starting gui...~%")
    (setf *clamps-gui-root* (merge-pathnames "www/" dir))
    (when (clog:is-running-p) (clog:shutdown))
    (uiop:run-program (format nil "mkdir -p ~a" svg-dir-path))
    (uiop:run-program (format nil "mkdir -p ~Asnd" (namestring dir)))
    (uiop:run-program (format nil "mkdir -p ~Aats" (namestring dir)))
    (setf ats-cuda:*ats-snd-dir* (merge-pathnames "snd/" dir))
    (setf ats-cuda:*ats-file-dir* (merge-pathnames "ats/" dir))
    (setf cm.svgd:svg-dir (merge-pathnames "www/svg/" dir))
    (let ((targetpath (namestring (merge-pathnames dir "/www"))))
      (dolist (dir-or-file '("js" "css" "favicon.ico" "start.html"))
        (let* ((subdirpath (format nil "www/~a" dir-or-file))
               (srcpath (namestring (asdf:system-relative-pathname :clog-dsp-widgets subdirpath))))
          (unless (uiop:probe-file* (merge-pathnames (format nil "~a~a" dir subdirpath)))
            (uiop:run-program (format nil "ln -s ~A ~A" srcpath targetpath))))))
    ;;    (clog:set-on-new-window #'clog::clamps-gui :path "/" :boot-file "/start.html")
    (setf (fdefinition 'clog-dsp-widgets::on-new-window) #'clog::clamps-gui)
    (clog:set-on-new-window #'clog-dsp-widgets::meters-window :path "/meters" :boot-file "/start.html")
    (clog:set-on-new-window  #'cm:svg-display :path "/svg-display" :boot-file "/start.html")
    (clog:set-on-new-window  #'ats-cuda-display:ats-display :path "/ats-display" :boot-file "/start.html")
    (when start-gui (progn (sleep 0.5) (clog-dsp-widgets:start-gui :directory (namestring dir))))))

;;; (uiop:probe-file* (namestring (merge-pathnames (pathname "/tmp/") "/www")))

(defparameter *mt-out01* nil)

#|
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
|#

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


(defun install-slime-hooks ()
  (swank:eval-in-emacs
   `(load ,(namestring
            (asdf:system-relative-pathname :clamps "elisp/incudine-hush.el"))))
  (swank:eval-in-emacs `(slime-repl-eval-string "(cm)")))

(defun install-sly-hooks ()
  (slynk:eval-in-emacs
   `(load ,(namestring
            (asdf:system-relative-pathname :clamps "elisp/incudine-hush-sly.el"))))
  (slynk:eval-in-emacs `(sly-interactive-eval "(cm)")))

(defun incudine-rts-hush ()
  (incudine:flush-pending)
  (dotimes (chan 16) (cm::sprout
                      (cm::new cm::midi-control-change :time 0
                        :controller 123 :value 127 :channel chan)))
  (incudine::node-free-unprotected)
;;;  (scratch::node-free-all)
  )

#+slynk
(defun install-standard-sly-hooks ()
  (slynk:eval-in-emacs
   '(progn
     (defun incudine-hush ()
       (interactive)
       (progn
         (sly-interactive-eval "(cm::rts-hush)"))
       "hush")
     (defun set-std-incudine-hush ()
       (interactive)
       (setq incudine-hush (symbol-function 'std-incudine-hush)))
     (defun set-cm-incudine-hush ()
       (interactive)
       (setq incudine-hush (symbol-function 'cm-incudine-hush)))
     (defun incudine-rt-start ()
       (interactive)
       (sly-interactive-eval "(incudine:rt-start)"))

     (defun incudine-rt-stop ()
       (interactive)
       (sly-interactive-eval "(incudine:rt-stop)"))
     (define-key lisp-mode-map (kbd "C-.") 'incudine-hush)
     (define-key lisp-mode-map (kbd "C-c C-.") 'incudine-rt-stop)
     (define-key lisp-mode-map (kbd "C-c M-.") 'incudine-rt-start))
   t)
  (cm::set-standard-hush))

(defun reset-logger-stream ()
  (setf incudine.util:*logger-stream* *error-output*))

(defparameter *sly-connected-hooks*
  (list #'cm::install-standard-sly-hooks #'cm::reset-logger-stream))

(defun cl-user::call-sly-connected-hooks ()
  (dolist (fn *sly-connected-hooks*) (funcall fn)))

(defun clamps-start-inkscape-osc ()
  (if (ou:port-available-p 1337)
       (progn
         (setf *osc-inkscape-export-in* (incudine.osc:open :port 1337 :host "127.0.0.1" :direction :input :protocol :udp))
         (incudine:make-osc-responder *osc-inkscape-export-in* "/inkscape/play" ""
                                      (lambda () (load #P"/tmp/incudine-export.lisp")
;;;                           (format t "~&~S~%~%" cl-user::*tmpsnd*)
                                        )
                                      )
         (incudine:recv-start *osc-inkscape-export-in*)
         :inkscape-osc-rcv-started)
       (warn "port 1337 already open!")))

(defun clamps-stop-inkscape-osc ()
  (incudine:remove-all-responders *osc-inkscape-export-in*)
  (if *osc-inkscape-export-in* (incudine.osc:close *osc-inkscape-export-in*))
  (setf *osc-inkscape-export-in* nil))


(defun clamps-start (&key (qsynth nil) (gui-root "/tmp") (start-gui t))
  (restart-inkscape-osc)
  (rts)
;;;  (unless (cm::rts?) (rts))
  ;;(make-mt-stream *mt-out01* *midi-out1* '(4 0))
  (if qsynth (restart-qsynth))
  ;;(setf *rts-out* *mt-out01*)
  (format t "~&midi initialized!~%")
  ;; (install-sly-hooks)
  (incudine:setup-io)
  (ats-cuda-display:ats-display-init)
  (start-doc-acceptor)
  (clamps-restart-gui gui-root :start-gui start-gui)
  (setf (fdefinition 'rts-hush) #'incudine-rts-hush)
  (clamps-logo))

#|

(stop-inkscape-osc)
(clamps-restart-gui "/tmp")
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
