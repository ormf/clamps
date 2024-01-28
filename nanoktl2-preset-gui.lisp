;;; 
;;; nanoktl2-preset-gui.lisp
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
(in-package :clog-midi-controller)

;;; gui

(defun create-nk2-numbox (parent midi-controller i margin)
  (let* ((min 0) (max 1)
         (precision 4)
         (css `(:background "#ddd" :text-align "center"  :font-size "2em" :height "1.3em" :margin ,margin))
         (binding (bind-ref-to-attr (aref (nk2-faders midi-controller) i) "value"))
         (var (b-ref binding))
         (attr (b-attr binding))
         (element (create-child
                   parent
                   (format nil "<o-numbox min=\"~a\" max=\"~a\" value=\"~a\" precision=\"~a\" ~@[~a~]>"
                           min max (get-val (b-ref binding)) precision
                           (clog-widgets::format-style css))))
         (unwatch
           (with-slots (cc-state cc-nums chan nk2-fader-modes) midi-controller
             (watch (lambda ()
                      (let ((fader-value (get-val (aref cc-state i)))
                            (hw-value (/ (aref
                                          (aref cl-midictl::*midi-cc-state* chan)
                                          (aref cc-nums i))
                                         127.0)))
                        (setf (style element :background-color)
                              (case (aref nk2-fader-modes i)
                                (:scale (if (= fader-value hw-value) "#ccc" "#7ef"))
                                (:jump "#ccc")
                                (:catch
                                    (if (= fader-value hw-value)
                                        "#aaffaa" "#ffaaaa"))))))))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (let ((*refs-seen* (list element)))
                     (if *debug* (format t "~&~%clog event from ~a: ~a~%" element
                                         (or (if (gethash "close" data) "close")
                                             (gethash attr data))))
                     (if (gethash "close" data)
                         (progn
                           (format t "closing numbox~%")
                           (setf (b-elist binding) (remove element (b-elist binding)))
                           (funcall unwatch)) ;;; cleanup: unregister elem.
                         (%set-val var (gethash attr data))))))
    element))

(defun create-nk2-preset-button (parent midi-controller button idx margin)
  (let* ((binding (bind-ref-to-attr button "highlight"))
         (label-off-binding (bind-ref-to-attr (elt (button-labels midi-controller) idx) "label-off"))
         (label-on-binding (bind-ref-to-attr (elt (button-labels midi-controller) idx) "label-on"))
         (css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.3em" :margin ,margin))
         (var (b-ref binding))
         (attr (b-attr binding))
         (label (b-ref label-off-binding))
         (element (create-child
                   parent
                   (format nil "<o-bang ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-toggle>"
                           (list
                            (opt-format-attr "value" (get-val var))
                            (opt-format-attr "flash" 0)
                            (opt-format-attr "label-off" (get-val label))
                            (opt-format-attr "label-on" (get-val label))
                            (opt-format-attr "background-off" "#888")
                            (opt-format-attr "background-on" "#f88")
                            (opt-format-attr "color-off" "black")
                            (opt-format-attr "color-on" "black")
                            (opt-format-attr "value-off" 0)
                            (opt-format-attr "value-on" 1))
                           (format-style css)
                           label)))
         ;; (unwatch
         ;;   (with-slots (preset-buttons cp-src s-buttons m-buttons tr-rec curr-bank) midi-controller
         ;;     (watch (let ((idx idx) ;;; preset-button-press behaviour
         ;;                  (btn (aref preset-buttons idx)))
         ;;              (lambda ()
         ;;                (get-val btn)
         ;;                (let ((cl-refs::*curr-ref* nil))
         ;;                  (format t "preset-button-pressed: ~a, ~a~%" (+ idx (* 16 curr-bank)) (get-val tr-rec))
         ;;                  (case (get-val tr-rec)
         ;;                    (0 (if cp-src
         ;;                           (progn
         ;;                             (format t "copy preset from ~a to ~a~%" cp-src (+ idx (* 16 curr-bank)))
         ;;                             (setf cp-src nil))
         ;;                           (format t "recall preset: ~a~%" (+ idx (* 16 curr-bank)))))
         ;;                    (1 (format t "store to preset: ~a~%" (+ idx (* 16 curr-bank))))
         ;;                    (2 (progn
         ;;                         (setf cp-src (+ idx (* 16 curr-bank)))
         ;;                         (if (< idx 8)
         ;;                             (set-val (aref s-buttons idx) 2)
         ;;                             (set-val (aref m-buttons (- idx 8)) 2))
         ;;                         (format t "set-cp-src to  ~a, ~a~%" (+ idx (* 16 curr-bank))
         ;;                         (if (< idx 8)
         ;;                             (get-val (aref s-buttons idx))
         ;;                             (get-val (aref m-buttons (- idx 8)))))
         ;;                         )))))))))
         )
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (push element (b-elist label-off-binding)) ;;; register the browser page's html elem for value updates.
    (push element (b-elist label-on-binding)) ;;; register the browser page's html elem for value updates.
    (let ((bang (elt (preset-buttons midi-controller) idx)))
      (set-on-data element ;;; react to clicks on the preset button in the browser
                   (lambda (obj data)
                     (declare (ignore obj))
                     (incudine.util:msg :info "~&~%clog event from ~a: ~a~%" element
                                        (or (if (gethash "close" data) "close")
                                            (gethash attr data)))
                     (cond ((gethash "close" data)
                            (progn
                              (incudine.util:msg :info "closing toggle~%")
                              (b-unregister element binding)
                              (b-unregister element label-off-binding)
                              (b-unregister element label-on-binding)))
                           (t (let ((*refs-seen* (list (list element attr))))
                                (trigger bang)))))))
    element))

(defun create-nk2-store-button (parent midi-controller button margin)
  (let* ((binding (bind-ref-to-attr button "highlight"))
         (css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.3em" :margin ,margin))
         (var (b-ref binding))
         (attr (b-attr binding))
         (label "sto")
         (element (create-child
                   parent
                   (format nil "<o-bang ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-toggle>"
                           (list
                            (opt-format-attr "value" (get-val var))
                            (opt-format-attr "flash" 0)
                            (opt-format-attr "label-off" label)
                            (opt-format-attr "label-on" label)
                            (opt-format-attr "background-off" "#888")
                            (opt-format-attr "background-on" "#f88")
                            (opt-format-attr "color-off" "black")
                            (opt-format-attr "color-on" "black")
                            (opt-format-attr "value-off" 0)
                            (opt-format-attr "value-on" 1))
                           (format-style css)
                           label))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to clicks in the store button on the browser page
                 (lambda (obj data)
                   (declare (ignore obj))
                   (incudine.util:msg :info "~&~%clog event from ~a: ~a~%" element
                                      (or (if (gethash "close" data) "close")
                                          (gethash attr data)))
                   (cond ((gethash "close" data)
                          (progn
                            (incudine.util:msg :info "closing toggle~%")
                            (b-unregister element binding)))
                         (t (let ((*refs-seen* (list (list element attr))))
                              (handle-store-button-press midi-controller))))))
    element))

(defun create-control-panel (parent midi-controller margin)
  (create-div parent :style "height: 3.05em;font-size: 3em" :content "nanoKontrol2")
  (let ((control-buttons (create-grid parent "nk2-ctl" "3/15")))
    (with-slots (track-left track-right
                 cycle set-marker marker-left marker-right
                 tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        midi-controller
      (loop
        for type in '(bang bang nil nil nil
                      bang nil bang bang bang
                      toggle toggle toggle toggle bang2)
        for label in '("<" ">" nil nil nil
                       "cycle" nil "set" "<" ">"
                       "p1" "p2" "p3" "p4" "sto")
        for button in (list
                       track-left track-right nil nil nil
                       cycle nil set-marker marker-left marker-right
                       tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        do (case type
             (bang
              (create-o-bang control-buttons (bind-ref-to-attr button "bang")
                             :background '("gray" "#ff8888") :label label
                             :css `(:height "1.3em" :font-size "1.5em" :text-align "center" :margin ,margin)))
             (bang2
              (create-nk2-store-button control-buttons midi-controller button margin))
             (toggle
              (create-o-toggle control-buttons (bind-ref-to-attr button "value")
                               :background '("gray" "#ff8888") :label label
                               :css `(:height "1.3em" :font-size 2em :text-align "center" :margin ,margin)))
             (otherwise (create-div control-buttons)))))))

(defun nanoktl2-preset-gui (id container &key (chan 5))
  (let ((midi-controller (or
                          (find-controller id)
                          (add-midi-controller 'nanoktl2-preset-midi
                                               :midi-input *midi-in1*
                                               :midi-output *midi-out1*
                                               :id id :chan chan)))
        (margin "0.2em"))
    (with-slots (fader s-buttons m-buttons r-buttons
                 track-left track-right
                 cycle set-marker marker-left marker-right
                 tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        midi-controller
      (let* (nk2-panel control-panel fader-panel)
        (setf nk2-panel (create-div container :class "nk2-panel"))
        (setf control-panel (create-div nk2-panel :style "flex-direction: column;"))
        (create-hide-button nk2-panel control-panel :background "transparent"
                                                    :label '(">" "<")
                                                    :css `(:margin ,margin) :auto-place :top)
        (create-control-panel control-panel midi-controller margin)
        (create-br container)
        (setf fader-panel (create-grid nk2-panel "nk2-fader" "8/40"))
        (dotimes (idx 16)
          (create-nk2-numbox fader-panel midi-controller idx margin))  
        (loop
          for button across (s-buttons midi-controller)
          for idx from 0
          do (create-nk2-preset-button fader-panel midi-controller button idx margin))
        (loop
          for button across (m-buttons midi-controller)
          for idx from 8
          do (create-nk2-preset-button fader-panel midi-controller button idx margin))
        (loop
          for button across (r-buttons midi-controller)
          for bank-no from 1
          do (create-o-toggle fader-panel (bind-ref-to-attr button "value")
                              :css `(:text-align "center" :user-select "none"
                                     :font-size "2em" :height "1.4em"
                                     :margin ,margin)
                              :background '("#888" "#f88")
                              :label (format nil "bank ~a" bank-no)))))))
