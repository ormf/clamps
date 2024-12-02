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

(set-on-new-window #'clamps-gui)
(set-on-new-window #'clog-dsp-widgets::meters-window :path "/meters")

(in-package :cl-midictl)

#|
(defun cl-midictl::ensure-default-midi-in (midi-in)
  (or (cm:ensure-jackmidi midi-input)
      (cm:ensure-jackmidi *midi-in1*)))

(defun cl-midictl::ensure-default-midi-out (midi-out)
  (or (cm:ensure-jackmidi midi-output)
      (cm:ensure-jackmidi *midi-out1*)))
|#

(in-package #:clamps)

(defparameter *svg-dir* nil)

(defun ensure-directory (dir)
  "return pathname of dir, ensuring it ends with a slash."
  (pathname (format nil "~A/" (if (stringp dir) dir (namestring dir)))))

(defvar *clamps-gui-root* nil)

(defun clamps-base-url ()
  "Return the base url to access the Clamps Gui (nicknamed
/<clamps-base-url>/ in this dictionary).

Its default location is http://localhost:54619.

/<clamps-base-url>/ on the browser side is corresponding to the
path /<clamps-gui-root>/, so an address named
/<clamps-base-url>/<file>/ will load the file located at
/<clamps-gui-root>/<file>/ as HTML into the browser window.

The location for the <<svg->browser><SVG Player Gui>> is at
/<clamps-base-url>/svg-display/ which translates to the URL

http://localhost:54619/svg-display

@See-also
clamps
clamps-restart-gui
clamps-gui-root
gui
meters
"
  (format nil "https://localhost:~d" clog:*clog-port*) )

(defun clamps-gui-root ()
  "Return the pathname of the Gui root directory. It is nicknamed
/<clamps-gui-root>/ throughout this dictionary.

/<clamps-gui-root>/ is the path corresponding to
/<clamps-base-url>/ on the browser side, so any file named /<file>/
put into the /<clamps-gui-root>/ directory can be accessed in the
browser at the address /<clamps-base-url>/<file>/.

@See-also
clamps
clamps-base-url
clamps-restart-gui
"
  *clamps-gui-root*)

(defun svg-gui-path (&optional file)
  "Return the full path of SVG file /file/ in the current GUI or the
base-gui-path of svg files if /file/ is not supplied.

@Arguments
file - A String designating the filename of the SVG file.

@Examples
(svg-gui-path)
;; => #P\"/tmp/www/svg/\"

(svg-gui-path \"test.svg\")
;; => #P\"/tmp/www/svg/test.svg\"

@See-also
clamps-gui-root
"
  (merge-pathnames (format nil "svg/~a" file) (ensure-directory (clamps-gui-root))))

(defun svg-gui-path (fname)
  "Return the path /<clamps-gui-root>/svg/fname/ as a string.

@Arguments
fname - String or Pathname.

@See-also

clamps-gui-root
"
  (namestring
   (merge-pathnames
    (format nil "svg/~a" fname)
    *clamps-gui-root*)))

(defun clamps-restart-gui (&key (gui-base "/tmp") (open t) (port 54619))
  "Reset the root directory of the Gui to /<gui-base>/www//, optionally
opening the Gui in a browser window. The command will create the
subdirectories /www//, /snd//, /ats// and /www/svg// in the
/<gui-base>/ directory, if they don't exist. /<gui-base>/www/svg// is
the file path for svg files used in the /svg-display/ page of the Gui.

@Arguments
:gui-base - String or Pathname where to put the /www/ subfolder for files
accessible by the gui (nicknamed /<clamps-gui-root>/ throughout
this dictionary). Defaults to //tmp/.

:open - is a flag indicating whether to open <<clamps-base-url>> in a
browser window after starting the gui.

Any files which need to be accessible by the Gui have to be put
into the /<clamps-gui-base>/www// subdirectory with their filenames
relative to this directory.

@See-also
clamps
clamps-base-url
clamps-gui-root
"
  (let* ((dir (pathname (ensure-directory gui-base)))
         (svg-dir-path (format nil "~Awww/svg/" (namestring dir))))
    (format t "(re)starting gui...~%")
    (setf *clamps-gui-root* (merge-pathnames "www/" dir))
    (when (clog:is-running-p) (clog:shutdown))
    (uiop:run-program (format nil "mkdir -p ~a" svg-dir-path))
    (uiop:run-program (format nil "mkdir -p ~Asnd" (namestring dir)))
    (uiop:run-program (format nil "mkdir -p ~Aats" (namestring dir)))
    (setf ats-cuda:*ats-snd-dir* (merge-pathnames "snd/" dir))
    (setf ats-cuda:*ats-file-dir* (merge-pathnames "ats/" dir))
    (setf cm.svgd:svg-dir (merge-pathnames "svg/" *clamps-gui-root*))
    (let ((targetpath (namestring (merge-pathnames dir "/www"))))
      (dolist (dir-or-file '("js" "css" "favicon.ico" "boot.html"))
        (let* ((subdirpath (format nil "www/~a" dir-or-file))
               (srcpath (namestring (asdf:system-relative-pathname :clog-dsp-widgets subdirpath))))
          (unless (uiop:probe-file* (merge-pathnames (format nil "~a~a" dir subdirpath)))
            (uiop:run-program (format nil "ln -s ~A ~A" srcpath targetpath))))))
    ;;    (clog:set-on-new-window #'clog::clamps-gui :path "/" :boot-file "/start.html")
    (setf (fdefinition 'clog-dsp-widgets::on-new-window) #'clog::clamps-gui)
    (clog:set-on-new-window #'clog-dsp-widgets::meters-window :path "/meters")
    (clog:set-on-new-window  #'cm:svg-display :path "/svg-display")
    (clog:set-on-new-window  #'ats-cuda-display:ats-display :path "/ats-display")
    (progn (sleep 0.5) (clog-dsp-widgets:start-gui :gui-base (namestring dir) :port port :open open))))

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
  "Sends an all-notes-off message to all channels of
/*​midi-out1​*/ and
calls <<node-free-unprotected>>.

@Note
This command is bound to the Keyboard Sequence /<C-.>/ in emacs.
"
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
  "Resets /incudine:*logger-stream*/ to /*​error-output​*/ Call this
function, if calls to /incudine.util:msg/ don't produce any output in
the REPL.

@Note
This function needs to be called if /Clamps/ is started from a Lisp
Image.
"  (setf incudine.util:*logger-stream* *error-output*))

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

(defun gui ()
  "Open the page at /<clamps-base-url>/ in a Browser.

@See-also
clamps-base-url
meters
"  (clog:open-browser))

(defun meters ()
  "Open the levelmeter page at /<clamps-base-url>/meters/ in a
Browser.

@See-also
clamps-base-url
gui
"  (clog:open-browser :url (format nil "http://127.0.0.1:~A/meters" clog::*clog-port*)))

(defun clamps-start (&key (gui-base "/tmp") (qsynth nil) (open-gui nil))
  "Entry function called by <<clamps>> to start the webserver for the
GUI, call <<rts>> to set up IO and MIDI, start the OSC responder for
Incudine, optionally start qsynth (Linux only) and open the gui in a
browser. This function should normally not be called by the user.

@Arguments
gui-base - The base path of the gui. <clamps-gui-root> will be <gui-base>/www/.
qsynth - Boolean indicating whether to start the qsynth softsynth (Linux only).
open-gui - Boolean indicating whether to open the gui in a Browser window.

@See-also
clamps
clamps-gui-root
rts
"
  (setf *package* (find-package :clamps))
   (restart-inkscape-osc)
;;; rts also initializes midi
  (rts)
;;;  (unless (cm::rts?) (rts))
  (if qsynth (restart-qsynth))
  ;;(setf *rts-out* *mt-out01*)
  (format t "~&midi initialized!~%")
  ;; (install-sly-hooks)
  (incudine:setup-io)
  (start-doc-acceptor)
  (clamps-restart-gui :gui-base gui-base :open open-gui)
  (ats-cuda-display:ats-display-init)
  (setf (fdefinition 'rts-hush) #'incudine-rts-hush)
  (reset-logger-stream)
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
