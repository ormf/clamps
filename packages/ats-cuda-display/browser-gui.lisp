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
        (play-state (get-val atsd.play))
        (bw (get-val atsd.bw))
        (play (get-val atsd.play)))
    (set-val atsd.play 0)
    (setf atsd.sound ats-snd)
    (unless (zerop play-state) (set-val atsd.play 0))
    (when reload (ats->svg atsd.sound))
    (set-val atsd.data svg-file :force t)
    (set-val atsd.x 0)
    (set-val atsd.bw (- bw 0.001))
    (set-val atsd.shift-x (/ (get-val atsd.width) 2))
    (let ((num-partials (ats-sound-partials atsd.sound)))
      (setf atsd.fmod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0))
      (setf atsd.amod (make-array num-partials
                                 :element-type 'incudine::sample
                                 :initial-element 1.0d0)))
    (set-val atsd.play play)
    (set-val atsd.bw bw)
    nil))

(defun start-browser-play ()
;;;  (format t "starting!~%")
  (when (and atsd.amod atsd.fmod)
    (if (ats-sound-bands atsd.sound)
        (incudine::sin-noi-rtc-synth* (float (or (first (get-val atsd.mousepos)) 0.0) 1.0d0) atsd.sound
                                      :amp-scale 0.1
                                      :res-bal (get-val atsd.res-balance)
                                      :amod atsd.amod
                                      :fmod atsd.fmod
                                      :head 200
                                      :action (lambda (n) (setf atsd.player-node-id (node-id n)))))))

(defun stop-browser-play ()
;;;  (format t "stopping!~%")
  (if atsd.player-node-id
      (incudine::free atsd.player-node-id))
  (setf atsd.player-node-id nil))

(progn
  (defparameter atsd.player-node-id nil)
  (defparameter atsd.sound nil)
  (defparameter atsd.fmod nil)
  (defparameter atsd.amod nil)
  (defparameter atsd.bw nil)
  (defparameter atsd.x nil)
  (defparameter atsd.shift-x nil)
  (defparameter atsd.width nil)
  (defparameter atsd.idx nil)
  (defparameter atsd.data nil)
  (defparameter atsd.crosshairs nil)
  (defparameter atsd.mousepos nil)
  (defparameter atsd.scale nil)
  (defparameter atsd.play nil)
  (defparameter atsd.contrast nil)
  (defparameter atsd.time nil)
  (defparameter atsd.freq nil)
  (defparameter atsd.pitch nil)
  (defparameter atsd.res-balance nil)
  (defparameter balance-watch nil)
  (defparameter data-watch nil)
  (defparameter play-watch nil)
  (defparameter pos-watch nil)
  (defparameter atsd.osc-node-id nil)
  (defparameter atsd.osc-amp nil)
  (defparameter atsd.osc-play nil)
  (defparameter osc-play-watch nil)
  (defparameter osc-freq-watch nil)
  (defparameter osc-amp-watch nil)
  )

(defun start-atsd-oscillator ()
  (osc~ (get-val atsd.freq) (get-val atsd.osc-amp)
        :head 200
        :action (lambda (n) (setf atsd.osc-node-id (node-id n)))))

(defun stop-atsd-oscillator ()
  (when atsd.osc-node-id
    (free atsd.osc-node-id)
    (setf atsd.osc-node-id nil)))

(defun ats-display-init ()
  (dolist (fn (list balance-watch data-watch play-watch pos-watch))
    (if fn (funcall fn)))
  (clear-bindings)
  (setf atsd.freq (make-ref 100))
  (setf atsd.pitch (make-computed
                   (lambda () (ou:ftom (get-val atsd.freq)))
                   (lambda (m) (set-val atsd.freq (ou:mtof m)))))
  (setf atsd.time (make-ref 0))
  (setf atsd.x (make-ref 0))
  (setf atsd.shift-x (make-ref 0))
  (setf atsd.width (make-ref 4))
  (setf atsd.idx (make-ref 0))
  (setf atsd.data (make-ref "atsd.snd.svg"))
  (setf atsd.crosshairs (make-ref 1))
  (setf atsd.contrast (make-ref 0.1))
  (setf atsd.mousepos (make-ref '(0 0)))
  (setf atsd.scale (make-ref 0.3))
  (setf atsd.res-balance (make-ref 0.5))
  (setf atsd.play (make-ref 0))
  (setf atsd.bw (make-ref 1))
  (setf atsd.osc-amp (make-ref 0))
  (setf atsd.osc-play (make-ref 0))
  (setf atsd.sound (ats-cuda:load-ats (merge-pathnames "ats-data/cl.ats" (asdf:system-source-directory :ats-cuda))))
  (setf balance-watch
        (watch (lambda ()
                 (let ((res-bal (get-val atsd.res-balance)))
                   (when atsd.player-node-id
                     (set-control atsd.player-node-id :res-bal res-bal))))))
  (setf data-watch
        (watch (lambda ()
                 (set-val atsd.shift-x (* (get-val atsd.scale) (/ (get-val atsd.width) 2)))
                 )))
  (setf play-watch
        (watch (lambda () (if (zerop (get-val atsd.play))
                         (stop-browser-play)
                         (start-browser-play)))))
  (setf osc-play-watch
        (watch (lambda () (if (zerop (get-val atsd.osc-play))
                         (stop-atsd-oscillator)
                         (start-atsd-oscillator)))))
  (setf osc-amp-watch
        (watch (lambda () (let ((amp (get-val atsd.osc-amp)))
                       (when atsd.osc-node-id
                         (incudine:set-control atsd.osc-node-id :amp amp))))))
  (setf osc-freq-watch
        (watch (lambda () (let ((freq (get-val atsd.freq)))
                       (when atsd.osc-node-id
                         (incudine:set-control atsd.osc-node-id :freq freq))))))
  (setf pos-watch
        (watch (lambda ()
                 (let* ((num-partials (ats-sound-partials atsd.sound))
                        (maxfreq (float (+ 100 (aref (ats-sound-frq-av atsd.sound) (1- num-partials)))))
                        (duration (float (ats-sound-dur atsd.sound))))  
                   (destructuring-bind (x y) (get-val atsd.mousepos)
                     (let ((bw (get-val atsd.bw)))
                       (when (and atsd.sound atsd.player-node-id)
                         (set-control atsd.player-node-id :soundpos (float x 1.0d0))
                         (let* ((frames (ats-sound-frames atsd.sound))
                                (soundpos x)
                                (mousefreq (float (* (max 0.0 (min y 1.0)) maxfreq))))
                           (set-val atsd.freq mousefreq)
                           (set-val atsd.time (* x duration))
                           (if (<= num-partials (length atsd.amod))
                               (loop for partial below num-partials
                                     for freq = (aref (ats-sound-frq atsd.sound)
                                                      partial
                                                      (min (1- frames)
                                                           (max 0
                                                                (round (* soundpos
                                                                          (1- frames))))))
                                     do (setf (aref atsd.amod partial)
                                              (float (ou:db->amp (* -18 (abs (/ (- freq mousefreq) (* 2 maxfreq bw))))) 1.0d0)))))
                         )
                       ))))))
  (ats->browser atsd.sound)
  nil)

(ats-display-init)

(defun atsd-set-keyboard-mouse-shortcuts (container atsd.svg atsd.play atsd.bw atsd.contrast atsd.res-balance)
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
           (clog:html-id atsd.play)
           (clog:html-id atsd.svg)
           (clog:html-id atsd.svg)
           (clog:html-id atsd.svg)))
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
                         (clog:html-id atsd.svg)))

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
           (clog:html-id atsd.svg)
           (clog:html-id atsd.contrast)
           (clog:html-id atsd.res-balance)
           (clog:html-id atsd.bw))))

(defun ats-display (body)
  "On-new-window handler."
  (let (controls atsd.svg atsd.play-toggle atsd.bw-slider atsd.contrast-slider
        atsd.res-bal-slider atsd.pitchbox atsd.freqbox atsd.timebox
        atsd.osc-play-toggle atsd.osc-amp-slider)
    (setf (title (clog::html-document body)) "ATS Cuda display")
    (setf atsd.svg
          (create-o-svg
           body (bind-refs-to-attrs atsd.width "width" atsd.x "cursor-pos" atsd.shift-x "shift-x" atsd.data "svg-file"
                                    atsd.scale "scale" atsd.crosshairs "crosshairs" atsd.mousepos "mousepos"
                                    atsd.bw "bandwidth" atsd.contrast "atsd.contrast")))
    ;; (create-o-radio body (bind-refs-to-attrs idx "value") :css '(:width "6em") :labels (list (loop for idx from 1 to 6 collect idx)) :num 6)
    (setf controls (create-div body :style "display: flex; height: 3em; margin-top: 0.5em"))
    (setf atsd.play-toggle
          (create-o-toggle controls (bind-refs-to-attrs atsd.play "value")
                           :css '(:font-size "2em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf atsd.contrast-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.contrast "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf atsd.bw-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.bw "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0.01 :max 1))
    (setf atsd.res-bal-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.res-balance "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 1))
    (setf atsd.timebox
          (create-o-numbox controls (bind-refs-to-attrs atsd.time "value") :min 0 :max 10
                           :css '(:margin-left "0.5em")))
    (setf atsd.pitchbox
          (create-o-numbox controls (bind-refs-to-attrs atsd.pitch "value") :min 0 :max 127
                           :css '(:margin-left "0.5em")))
    (setf atsd.freqbox
          (create-o-numbox controls (bind-refs-to-attrs atsd.freq "value") :min 0 :max 10000
                           :css '(:margin-left "0.5em")))
    (setf atsd.osc-play-toggle
          (create-o-toggle controls (bind-refs-to-attrs atsd.osc-play "value")
                           :css '(:font-size "2em" :margin-left "0.5em" :width "3em" :display "block") :label '("off" "on") :background '("transparent" "#8f8")))
    (setf atsd.osc-amp-slider
          (create-o-slider controls (bind-refs-to-attrs atsd.osc-amp "value")
                           :width "4em" :css '(:margin-left "0.5em") :height "88%" :direction :right :min 0 :max 0.01))
    (atsd-set-keyboard-mouse-shortcuts body atsd.svg atsd.play-toggle atsd.bw-slider atsd.contrast-slider atsd.res-bal-slider)))

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
