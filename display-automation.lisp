;;; 
;;; display-automation.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022-24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-user)

(defpackage #:clamps.svgd
  (:use #:cl)
  (:export #:shift #:cursor-pos #:width #:scale #:seq #:inverse #:timescale
           #:piano-roll #:staff-systems #:bar-lines #:idx #:data #:svg-file
           #:transport #:auto-return #:play-watch #:data-watch #:timescale-watch
           #:svg-display
           #:svg-dir #:*play-hooks* #:*stop-hooks*))

(in-package :clamps.svgd)

(defparameter cursor-pos nil)
(defparameter shift nil)
(defparameter width nil)
(defparameter scale nil)
(defparameter seq nil)
(defparameter inverse nil)
(defparameter timescale nil)
(defparameter piano-roll nil)
(defparameter staff-systems nil)
(defparameter bar-lines nil)
(defparameter idx nil)
(defparameter svg-file nil)
(defparameter transport nil)
(defparameter auto-return (cl-refs:make-ref 0))
(defparameter play-watch nil)
(defparameter timescale-watch nil)
(defparameter data-watch nil)
(defparameter svg-dir nil)

(defparameter *play-hooks* nil)
(defparameter *stop-hooks* nil)

(defun tempo->svg-timescale (arg1 &optional arg2)
  "Convert tempo setting into a timescale in seconds per svg unit for
svg playback in the clamps Gui. The svg unit is a 1/16th rhythm.

@Arguments
arg1 - Number denoting either a rhythm value (1/4 for a quarter note), or a bpm value if arg2 isn't supplied.
arg2 - Number denoting the bets per minute.

@Examples

(tempo->svg-timescale 1/4 60) -> 1/4

(tempo->svg-timescale 1/2 60) -> 1/8

(tempo->svg-timescale 60) -> 1/4
"
  (let ((bpm (or arg2 arg1))
        (rh (if arg2 arg1 1/4)))
    (/ 15/4 (* rh bpm))))


(in-package :cm)

;;; (export 'sfz 'cm)

(defun beat->time (beat &key (factor 4/10))
  (if beat (* beat factor)))

(defun time->beat (time &key (factor 4/10))
  (if time (/ time factor)))

;;; (chan->color 4)

(defun get-first-in-region (evts region timescale)
  (loop
    for e in evts
    until (>= (object-time e) (beat->time (* 16 (- (first region) 1)) :factor timescale))
    finally (return (time->beat (object-time e) :factor timescale))))

#|
(defun free-voice (num)
  (setf *curr-voices* (delete num *curr-voices*))
  (display-send num :vn-nonvib-mf-4c -500))

(defun free-all-voices ()
  (dotimes (n 16)
    (funcall (free-voice n)))
  (setf *curr-voices* nil))
|#

(defun get-timescale (&rest tempo)
  "calc svg timescale from tempo"
  (/ 15/4 (apply #'* tempo)))

(defun object-end (obj)
  (typecase obj
    (poolevt
     (let* ((end (sv obj :end))
            (buffer (of-incudine-dsps:lsample-buffer (sv obj :lsample)))
            (bufdur (float (/ (incudine::buffer-frames buffer)
                              (incudine::buffer-sample-rate buffer))
                           1.0)))
       (+ (object-time obj) (* (sv obj :stretch) (if (zerop end) bufdur (min end bufdur))))))
    (t (+ (object-time obj) (sv obj :duration)))))

(defun trim-start (obj curr-pos obj-end)
  (typecase obj
    (poolevt
     (setf (sv obj :start) (/ (- curr-pos (object-time obj)) (sv obj :stretch)))
     (sv* obj :stretch (get-val clamps.svgd:timescale)))
    (t (setf (sv obj :duration)
             (* (get-val clamps.svgd:timescale) (- obj-end curr-pos)))))
;;  (break "~a" obj)
  (values))

(defun seq-play (obj)
  (let* ((evts (subobjects obj))
;;;         (offs (get-first-in-region evts region timescale))
         )
    (if evts 
        (progn
          (dolist (hook clamps.svgd:*play-hooks*) (funcall hook))
          (let ((curr-pos (get-val clamps.svgd:shift)))
            (dolist (obj evts)
              (let ((obj-end (object-end obj)))
                (when (>= obj-end curr-pos)
                  (let ((obj (copy-object obj)))
                    (if (< (object-time obj) curr-pos)
                        (progn
                          (trim-start obj curr-pos obj-end)
                          (setf (sv obj :time) 0))
                        (progn
                          (setf (sv obj :time)
                                (float (* (get-val clamps.svgd:timescale)
                                          (- (sv obj :time) curr-pos))))
                          (typecase obj
                            (poolevt (sv* obj :stretch (get-val clamps.svgd:timescale)))
                            (t (sv* obj :duration (get-val clamps.svgd:timescale))))))
                    (incudine.util::msg :info "~a~%" obj)
                    (sprout obj))
                  ))
              ))
          ;;          (browser-play (* offs 6.041) :tscale (/ 1/8 6.041))
          ))))

(defmacro sv- (obj slot val &body more) (svaux obj '- slot val more))

(defun init-svg-display ()
  "(re)initalize all ref-objects."
  (clear-bindings)
  (when clamps.svgd:play-watch (funcall clamps.svgd:play-watch))
  (when clamps.svgd:data-watch (funcall clamps.svgd:data-watch))
  (setf clamps.svgd:cursor-pos (make-ref 0.5))
  (setf clamps.svgd:inverse (make-ref 0))
  (setf clamps.svgd:shift (make-ref 0))
  (setf clamps.svgd:seq (make-ref nil))
  (setf clamps.svgd:width (make-ref 0))
  (setf clamps.svgd:scale (make-ref 1))
  (setf clamps.svgd:timescale (make-ref (get-timescale 1/4 96)))
  (setf clamps.svgd:piano-roll (make-ref 1))
  (setf clamps.svgd:staff-systems (make-ref 1))
  (setf clamps.svgd:bar-lines (make-ref 1))
  (setf clamps.svgd:idx (make-ref 0))
  (setf clamps.svgd:transport (make-ref 0))
  (setf clamps.svgd:svg-file (make-ref ""))
  nil)

(defun set-keyboard-shortcuts (container transport-toggle)
  (clog:js-execute
   container
   (format nil "document.onkeydown = function (event) {
  if (event.which == 32 || event.code == 'Space') {
    let transportToggle = document.getElementById('~a'); 
    let currValue = transportToggle.getAttribute('value');
    transportToggle.externalValueChange = false;
    if (currValue == 0) {
      transportToggle.setAttribute('value', 1);
    }
    else {
      transportToggle.setAttribute('value', 0);
    }
  }
};
"
           (clog:html-id transport-toggle))))

(defun svg-display (body)
  "On-new-window handler."
  (let (transport-toggle controls)
    (setf (clog:title (clog:html-document body)) "SVG Player")
    (setf (clog:style body :overflow) "auto")
    (create-o-svg
     body (bind-refs-to-attrs clamps.svgd:width "width"
                              clamps.svgd:cursor-pos "cursor-pos"
                              clamps.svgd:shift "shift-x"
                              clamps.svgd:svg-file "svg-file"
                              clamps.svgd:scale "scale"
                              clamps.svgd:piano-roll "piano-roll"
                              clamps.svgd:staff-systems "staff-systems"
                              clamps.svgd:bar-lines "bar-lines"
                              clamps.svgd:inverse "inverse")
;;;     :css '(:height "98rem")
     )
    (create-o-slider body (bind-refs-to-attrs clamps.svgd:shift "value" clamps.svgd:width "max")
                     :min 0 :max 200 :direction :right
                     :css `(:display "inline-block" :height "1em" :width "100%"))
    (setf controls (clog:create-div body :css '(:display "flex" :width "100%")))
    (setf transport-toggle
          (create-o-toggle controls (bind-refs-to-attrs clamps.svgd:transport "value")
                           :label '("play" "stop") :background '("transparent" "#8f8")
                           :css `(:display "inline-block" :height "1.2em" :width "3em")))
    (create-o-toggle controls (bind-refs-to-attrs clamps.svgd:auto-return "value")
                     :label '("rtn") :css `(:display "inline-block" :height "1.2em" :width "3em" :margin-left "0.2em"))
    (create-o-toggle controls (bind-refs-to-attrs clamps.svgd:piano-roll "value")
                     :label '("pno") :css `(:display "inline-block" :height "1.2em" :width "3em" :margin-left "0.2em"))
    (create-o-toggle controls (bind-refs-to-attrs clamps.svgd:staff-systems "value")
                     :label '("stf") :css `(:display "inline-block" :height "1.2em" :width "3em" :margin-left "0.2em"))
    (create-o-toggle controls (bind-refs-to-attrs clamps.svgd:bar-lines "value")
                     :label '("bar") :css `(:display "inline-block" :height "1.2em" :width "3em" :margin-left "0.2em"))
    (set-keyboard-shortcuts body transport-toggle))) 

(clog:set-on-new-window 'svg-display :path "/svg-display")

;; Initialize the CLOG system with a boot file which contains the
;; static js files. For customized uses copy the "www" subdirectory of
;; the repository to your local project and adjust :static-root
;; accordingly
(defun start-svg-display (&key (static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))))
  (clear-bindings) ;;; start from scratch
  (clog:initialize nil
              :port 54619
              :static-root static-root
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (clog:set-on-new-window  'svg-display :path "/svg-display" :boot-file "/start.html")
  (clog:open-browser :url "http://127.0.0.1:54619/svg-display"))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

;;; (start-svg-display)

;; (set-val cursor-pos 0.5)
;; (set-val svg-width 8000)

;; (set-val svg-scale 9.5)

;;; (set-val svg-timescale 0.125) 

(defun svg-play ()
  (labels ((inner (time)
             (unless (zerop (get-val clamps.svgd:transport))
               (when (> (get-val clamps.svgd:shift) (+ 2 (get-val clamps.svgd:width))) (set-val clamps.svgd:transport 0))
               (set-val clamps.svgd:shift (+ (get-val clamps.svgd:shift)
                                         (* 1.067 (float (/ 1/64 (get-val clamps.svgd:timescale))))))
                 (let ((next (+ time 1/60)))
                   (cm:at next #'inner next)))))
    (inner (cm:now))))

;;; (funcall my-watch)

(defun bool->int (bool)
  "convert t to 1 and nil to 0."
  (if bool 1 0))

(defun svg->browser (svg-file &key (bar-lines t) (staff-systems t)
                                (piano-roll nil) (scale 1)
                                (timescale 1/32) (inverse nil)
                                (reload t))
  "Display =svg-file= in the SVG Player Gui, located at
/<clamps-base-url>/svg-display/.

@Arguments
svg-file - String naming the svg-file to display/play. The
filename is interpreted relative to the /<clamps-gui-root>/svg/
directory.

:bar-lines - Boolean indicating whether to display barlines.

:staff-systems - Boolean indicating whether to display staff systems.

:piano-roll - Boolean indicating whether to display a piano roll.

:scale - Positive Number denoting the zoom factor of the graphic.

:timescale - Positive number denoting The timescale for playback.

:inverse - 0 or 1 indicating inverse colors.

@Example

(events
 (loop
   for i below 10
   collect (new midi :time (* 0.1 i) :keynum (between 60 72) :duration 0.1))
 (svg-gui-path \"test.svg\"))

(svg->browser \"test.svg\")

;; Then open a browser at http://127.0.0.1:54619/svg-display

@See-also
clamps:clamps-svg.rts
svg-gui-path
"
  (if (string= (namestring svg-file) (file-namestring svg-file))
      (progn
        (unless (probe-file (clamps:svg-gui-path (file-namestring svg-file)))
          (error "~%File not found:~%~a" (clamps:svg-gui-path (file-namestring svg-file))))
        (set-val clamps.svgd:transport 0)
        (set-val clamps.svgd:svg-file (file-namestring svg-file) :force reload)
        (set-val clamps.svgd:piano-roll (bool->int piano-roll))
        (set-val clamps.svgd:staff-systems (bool->int staff-systems))
        (set-val clamps.svgd:bar-lines (bool->int bar-lines))
        (set-val clamps.svgd:scale scale)
        (set-val clamps.svgd:inverse (bool->int inverse))
        (if clamps.svgd:data-watch (funcall clamps.svgd:data-watch))
        (if clamps.svgd:play-watch (funcall clamps.svgd:play-watch))
        (if clamps.svgd:timescale-watch (funcall clamps.svgd:timescale-watch))
        (set-val clamps.svgd:timescale timescale)
        (setf clamps.svgd:timescale-watch (watch (lambda () (setf *svg-x-scale* (get-val clamps.svgd:timescale)))))
        (setf clamps.svgd:play-watch
              (watch (let ((last-pos 0))
                       (lambda () (if (zerop (get-val clamps.svgd:transport))
                                 (let (cl-refs::*curr-ref*)
;;;                             (format t "stopping~%")
                                   (dolist (hook clamps.svgd:*stop-hooks*) (funcall hook))
                                   (at (+ (now 0)) #'rts-hush)
                                   (unless (zerop (get-val clamps.svgd:auto-return))
                                     (set-val clamps.svgd:shift last-pos)))
                                 (alexandria:if-let ((seq (get-val clamps.svgd:seq)))
                                   (let (cl-refs::*curr-ref*)
;;;                                        (format t "relocating~%")
                                     (setf last-pos (get-val clamps.svgd:shift))
                                     (dolist (hook clamps.svgd:*play-hooks*) (funcall hook))
                                     (seq-play seq)
                                     (svg-play))
                                   (error "seq not present: ~a" svg-file)))))))
        (setf clamps.svgd:data-watch
              (watch (lambda ()
                       (let ((filename (get-val clamps.svgd:svg-file)))
                         (unless (string= filename "")
                           (format t "~&importing: ~a~%" (get-val clamps.svgd:svg-file))
                           (let ((seq
                                   (cm:import-events
                                    (namestring
                                     (merge-pathnames clamps.svgd:svg-dir filename))
                                    :x-scale 1)))
                             (set-val clamps.svgd:timescale timescale)
                             (format t "~&seq: ~a~%" seq)
                             (set-val clamps.svgd:seq seq)
                             (when seq
                               (setf (container-subobjects seq)
                                     (sort (subobjects seq) #'< :key #'object-time)))
                             (values))))))))
      (error "Absolute filename provided to svg->browser:~%~S~%Only relative filenames are allowed."
             svg-file)))

(init-svg-display)

(export '(svg-display) 'cm)

#|

(set-val clamps.svgd:piano-roll 0)
(set-val clamps.svgd:scale 9.5)
(set-val clamps.svgd:bar-lines 0)
(set-val clamps.svgd:staff-systems 0)

(set-val clamps.svgd:timescale 0.125)
(set-val clamps.svgd:timescale 0.25)
(get-val clamps.svgd:width)

(progn
 (set-val clamps.svgd:cursor-pos 0.2)
 (set-val clamps.svgd:cursor-pos 0.5))
;;; (ql:quickload '(clack websocket-driver alexandria cm-all))

|#
