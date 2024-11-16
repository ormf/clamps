;;; 
;;; browser-gui.lisp
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

(in-package :ats-cuda-display)

(sb-ext:defglobal *sine1024* (make-buffer 1024 :fill-function (gen:partials '(1))))

(defun restore-tables ()
  (setq *sine1024* (make-buffer 1024 :fill-function (gen:partials '(1)))))

(incudine.vug:dsp! osc~ (freq amp (buf buffer))
  (:defaults 440 0.1 *SINE1024*)
  (incudine.vug:foreach-frame
    (let ((sig (incudine.vug:osc buf freq amp 0 :linear)))
      (incudine.vug:out sig sig))))

;;; (start-ats-oscillator)

(defparameter *ats-snd-directory* (pathname "/tmp/"))

(defun ats->browser (ats-snd &key (reload t))
     "Display the ats struct /ats-snd/ graphically in the interactive ATS
Player located at /<clamps-base-url>/ats-display/ in the Gui.

@Arguments
ats-snd - The ats struct to display.

@See-also
clamps-base-url
"
  (let ((svg-file (format nil "~a.svg"
                          (string-downcase (ats-sound-name ats-snd))))
        (play-state (get-val ats-play))
        (bw (get-val ats-bw))
        (play (get-val ats-play)))
    (set-val ats-play 0)
    (setf ats-sound ats-snd)
    (unless (zerop play-state) (set-val ats-play 0))
    (when reload (ats->svg ats-snd))
    (set-val ats-data svg-file :force t)
    (set-val ats-x 0)
    (set-val ats-bw (- bw 0.001))
    (set-val ats-shift-x (/ (get-val ats-width) 2))
    (let ((num-partials (ats-sound-partials ats-snd)))
      (setf ats-fmod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0))
      (setf ats-amod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0)))
    (set-val ats-play play)
    (set-val ats-bw bw)
    nil))

(defun start-browser-play ()
;;;  (format t "starting!~%")
  (when (and ats-amod ats-fmod)
    (setf ats-player-node-id (incudine:next-node-id))
    (if (ats-sound-bands ats-sound)
        (incudine::sin-noi-rtc-synth* (float (or (first (get-val ats-mousepos)) 0.0) 1.0d0) ats-sound
                                      :amp-scale 0.1
                                      :id ats-player-node-id
                                      :res-bal (get-val ats-res-balance)
                                      :amod ats-amod
                                      :fmod ats-fmod
                                      :head 200))))

(defun stop-browser-play ()
;;;  (format t "stopping!~%")
  (if ats-player-node-id
      (incudine::free ats-player-node-id))
  (setf ats-player-node-id nil))

(progn
  (defparameter ats-player-node-id nil)
  (defparameter ats-sound nil)
  (defparameter ats-fmod nil)
  (defparameter ats-amod nil)
  (defparameter ats-bw nil)
  (defparameter ats-x nil)
  (defparameter ats-shift-x nil)
  (defparameter ats-width nil)
  (defparameter ats-idx nil)
  (defparameter ats-data nil)
  (defparameter ats-crosshairs nil)
  (defparameter ats-mousepos nil)
  (defparameter ats-scale nil)
  (defparameter ats-play nil)
  (defparameter ats-contrast nil)
  (defparameter ats-time nil)
  (defparameter ats-freq nil)
  (defparameter ats-pitch nil)
  (defparameter ats-res-balance nil)
  (defparameter balance-watch nil)
  (defparameter data-watch nil)
  (defparameter play-watch nil)
  (defparameter pos-watch nil)
  (defparameter ats-osc-node-id nil)
  (defparameter ats-osc-amp nil)
  (defparameter ats-osc-play nil)
  (defparameter osc-play-watch nil)
  (defparameter osc-freq-watch nil)
  (defparameter osc-amp-watch nil)
  )

(defun start-ats-oscillator ()
  (setf ats-osc-node-id (incudine:next-node-id))
  (osc~ (get-val ats-freq) (get-val ats-osc-amp) :head 200 :id ats-osc-node-id))

(defun stop-ats-oscillator ()
  (free ats-osc-node-id)
  (setf ats-osc-node-id nil))

(defun ats-display-init ()
  (dolist (fn (list balance-watch data-watch play-watch pos-watch))
    (if fn (funcall fn)))
  (clear-bindings)
  (setf ats-freq (make-ref 100))
  (setf ats-pitch (make-computed
                   (lambda () (ou:ftom (get-val ats-freq)))
                   (lambda (m) (set-val ats-freq (ou:mtof m)))))
  (setf ats-time (make-ref 0))
  (setf ats-x (make-ref 0))
  (setf ats-shift-x (make-ref 0))
  (setf ats-width (make-ref 4))
  (setf ats-idx (make-ref 0))
  (setf ats-data (make-ref "ats-snd.svg"))
  (setf ats-crosshairs (make-ref 1))
  (setf ats-contrast (make-ref 0.1))
  (setf ats-mousepos (make-ref '(0 0)))
  (setf ats-scale (make-ref 1))
  (setf ats-res-balance (make-ref 0.5))
  (setf ats-play (make-ref 0))
  (setf ats-bw (make-ref 1))
  (setf ats-osc-amp (make-ref 0))
  (setf ats-osc-play (make-ref 0))
  (ats-cuda:ats-load (merge-pathnames "ats-data/cl.ats" (asdf:system-source-directory :ats-cuda)) 'ats-sound)
  (setf balance-watch
        (watch (lambda ()
                 (let ((res-bal (get-val ats-res-balance)))
                   (when ats-player-node-id
                     (set-control ats-player-node-id :res-bal res-bal))))))
  (setf data-watch
        (watch (lambda ()
                 (set-val ats-shift-x (/ (get-val ats-width) 2))
                 )))
  (setf play-watch
        (watch (lambda () (if (zerop (get-val ats-play))
                         (stop-browser-play)
                         (start-browser-play)))))
  (setf osc-play-watch
        (watch (lambda () (if (zerop (get-val ats-osc-play))
                         (stop-ats-oscillator)
                         (start-ats-oscillator)))))
  (setf osc-amp-watch
        (watch (lambda () (let ((amp (get-val ats-osc-amp)))
                       (when ats-osc-node-id
                         (incudine:set-control ats-osc-node-id :amp amp))))))
  (setf osc-freq-watch
        (watch (lambda () (let ((freq (get-val ats-freq)))
                       (when ats-osc-node-id
                         (incudine:set-control ats-osc-node-id :freq freq))))))
  (setf pos-watch
        (watch (lambda ()
                 (let* ((num-partials (ats-sound-partials ats-sound))
                        (maxfreq (float (+ 100 (aref (ats-sound-frq-av ats-sound) (1- num-partials)))))
                        (duration (float (ats-sound-dur ats-sound))))  
                   (destructuring-bind (x y) (get-val ats-mousepos)
                     (let ((bw (get-val ats-bw)))
                       (when (and ats-sound ats-player-node-id)
                         (set-control ats-player-node-id :soundpos (float x 1.0d0))
                         (let* ((frames (ats-sound-frames ats-sound))
                                (soundpos x)
                                (mousefreq (float (* (max 0.0 (min y 1.0)) maxfreq))))
                           (set-val ats-freq mousefreq)
                           (set-val ats-time (* x duration))
                           (if (<= num-partials (length ats-amod))
                               (loop for partial below num-partials
                                     for freq = (aref (ats-sound-frq ats-sound)
                                                      partial
                                                      (min (1- frames)
                                                           (max 0
                                                                (round (* soundpos
                                                                          (1- frames))))))
                                     do (setf (aref ats-amod partial)
                                              (float (ou:db->amp (* -18 (abs (/ (- freq mousefreq) (* 2 maxfreq bw))))) 1.0d0))))))))))))
  (ats->browser ats-sound)
  nil)

(defun ats-set-keyboard-mouse-shortcuts (container ats-svg ats-play ats-bw ats-contrast ats-res-balance)
  "set key and mouse wheel handlers in the ats-display gui."
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
  if (event.shiftKey) {
    let atsSvg = document.getElementById('~a'); 
    if (!atsSvg.shiftKey ) {
        atsSvg.shiftKey = true;
//        console.log('shiftKey pressed');
    }
  }
  if (event.altKey) {
    let atsSvg = document.getElementById('~a'); 
    if (!atsSvg.altKey ) {
        atsSvg.altKey = true;
//        console.log('altKey pressed');
    }
  }
  if (event.ctrlKey) {
    let atsSvg = document.getElementById('~a'); 
    if (!atsSvg.ctrlKey ) {
        atsSvg.ctrlKey = true;
//        console.log('ctrlKey pressed');
    }
  }
};
"
           (clog:html-id ats-play)
           (clog:html-id ats-svg)
           (clog:html-id ats-svg)
           (clog:html-id ats-svg)))
  (clog:js-execute
   container
   (format nil "document.onkeyup = function (event) {
    let atsSvg = document.getElementById('~a'); 
    if (!event.shiftKey && atsSvg.shiftKey) {
      atsSvg.shiftKey = false;
      console.log('shiftKey released');
    }
    if (!event.altKey && atsSvg.altKey) {
      atsSvg.altKey = false;
      console.log('altKey released');
    }
    if (!event.ctrlKey && atsSvg.ctrlKey) {
      atsSvg.ctrlKey = false;
      console.log('ctrlKey released');
    }
};
"
                         (clog:html-id ats-svg)))

  (clog:js-execute
   container
   (format nil "document.onwheel = function (event) {
     let atsSvg = document.getElementById('~a');
     if (atsSvg.shiftKey && !atsSvg.altKey && !atsSvg.ctrlKey) {
       let contrastSlider = document.getElementById('~a');
       let newValue = Math.min(1, Math.max (0, parseFloat(contrastSlider.getAttribute(\"value\")) + event.deltaY/3000));
  //     console.log('contrast: ', newValue, 'slider: ', contrastSlider.getAttribute(\"value\"), 'dY: ', event.deltaY/1000);
       contrastSlider.setAttribute(\"value\", newValue);
       $(contrastSlider).trigger(\"data\", { value: parseFloat(newValue) });
     }
     else {
       if (atsSvg.altKey && atsSvg.ctrlKey && !atsSvg.shiftKey) {
         let resBalSlider = document.getElementById('~a');
         let newValue = Math.min(1, Math.max (0, parseFloat(resBalSlider.getAttribute(\"value\")) + event.deltaY/-3000));
  //       console.log('res-balance: ', newValue, 'slider: ', resBalSlider.getAttribute(\"value\"), 'dY: ', event.deltaY/-3000);
         resBalSlider.setAttribute(\"value\", newValue);
         $(resBalSlider).trigger(\"data\", { value: parseFloat(newValue) });
       }
       else {
         if (!atsSvg.shiftKey && !atsSvg.altKey && !atsSvg.ctrlKey)
         {
           let bwSlider = document.getElementById('~a');
           let newValue = Math.min(1, Math.max (0.01, parseFloat(bwSlider.getAttribute(\"value\")) + event.deltaY/-3000));
           bwSlider.setAttribute(\"value\", newValue);
           $(bwSlider).trigger(\"data\", { value: parseFloat(newValue) });
         }
       }
     }
};
"
           (clog:html-id ats-svg)
           (clog:html-id ats-contrast)
           (clog:html-id ats-res-balance)
           (clog:html-id ats-bw))))

(defun ats-display (body)
  "On-new-window handler."
  (let (controls ats-svg ats-play-toggle ats-bw-slider ats-contrast-slider
        ats-res-bal-slider ats-pitchbox ats-freqbox ats-timebox
        ats-osc-play-toggle ats-osc-amp-slider)
    (setf (title (clog::html-document body)) "ATS Cuda display")
    (setf ats-svg
          (create-o-svg
           body (bind-refs-to-attrs ats-width "width" ats-x "cursor-pos" ats-shift-x "shift-x" ats-data "svg-file"
                                    ats-scale "scale" ats-crosshairs "crosshairs" ats-mousepos "mousepos"
                                    ats-bw "bandwidth" ats-contrast "ats-contrast")))
    ;; (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
    (setf controls (create-div body :style "display: flex; height: 3em; margin-top: 0.5em"))
    (setf ats-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs ats-play "value")
                           :css '(:font-size "2em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf ats-contrast-slider
          (create-o-slider controls (bind-refs-to-attrs ats-contrast "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf ats-bw-slider
          (create-o-slider controls (bind-refs-to-attrs ats-bw "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0.01 :max 1))
    (setf ats-res-bal-slider
          (create-o-slider controls (bind-refs-to-attrs ats-res-balance "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf ats-timebox
          (create-o-numbox controls (bind-refs-to-attrs ats-time "value") :min 0 :max 10
                           :css '(:margin-left "0.5em")))
    (setf ats-pitchbox
          (create-o-numbox controls (bind-refs-to-attrs ats-pitch "value") :min 0 :max 127
                           :css '(:margin-left "0.5em")))
    (setf ats-freqbox
          (create-o-numbox controls (bind-refs-to-attrs ats-freq "value") :min 0 :max 10000
                           :css '(:margin-left "0.5em")))
    (setf ats-osc-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs ats-osc-play "value")
                           :css '(:font-size "2em" :margin-left "0.5em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf ats-osc-amp-slider
          (create-o-slider controls (bind-refs-to-attrs ats-osc-amp "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 0.01))
    (ats-set-keyboard-mouse-shortcuts body ats-svg ats-play-toggle ats-bw-slider ats-contrast-slider ats-res-bal-slider)))

(defun on-new-ats-window (body)
  (ats-display body))

(set-on-new-window #'on-new-ats-window :path "/ats-display" :boot-file "/start.html")

(defun ats-display-start ()
  (clear-bindings) ;;; start from scratch
  (initialize #'ats-display
              :port 8080
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-dsp-widgets))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

;;; (ats-display-start)
