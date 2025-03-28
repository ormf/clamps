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
                           (clog-dsp-widgets::format-style css))))
         (unwatch
           (with-slots (cc-state cc-nums chan nk2-fader-modes) midi-controller
             (watch (lambda ()
                      (let ((fader-value (get-val (aref cc-state i)))
                            (hw-value (/ (get-val
                                          (aref
                                           (aref cl-midictl::*midi-cc-state* chan)
                                           (aref cc-nums i)))
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
                     (incudine.util:msg :debug "~&~%clog event from ~a: ~a~%" element
                              (or (if (gethash "close" data) "close")
                                  (gethash attr data)))
                     (if (gethash "close" data)
                         (progn
;;;                           (format t "closing numbox~%")
                           (setf (b-elist binding) (remove element (b-elist binding)))
                           (funcall unwatch)) ;;; cleanup: unregister elem.
                         (%set-val var (gethash attr data))))))
    element))

(defun create-nk2-preset-button (parent button label-button margin)
  (create-o-bang parent (bind-refs-to-attrs button "bang" button "highlight"
                                            label-button "label-off" label-button "label-on")
                 :background '("gray" "#ff8888")
                 :css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.3em" :margin ,margin)))

(defun create-nk2-preset-bank-button (parent button label margin)
  (create-o-bang parent (bind-refs-to-attrs button "bang" button "highlight")
                 :label label
                 :flash 0
                 :background '("gray" "#ff8888")
                 :css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.3em" :margin ,margin)))


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
                      bang bang bang bang bang)
        for label in '("<" ">" nil nil nil
                       "cycle" nil "set" "<" ">"
                       "p1" "p2" "p3" "p4" "sto")
        for button in (list
                       track-left track-right nil nil nil
                       cycle nil set-marker marker-left marker-right
                       tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        do (case type
             (bang
              (create-o-bang control-buttons (bind-refs-to-attrs button "bang" button "highlight")
                             :background '("gray" "#ff8888") :label label
                             :css `(:height "1.3em" :font-size "1.5em" :text-align "center" :margin ,margin)))
             ;; (toggle
             ;;  (create-o-toggle control-buttons (bind-refs-to-attrs button "value")
             ;;                   :background '("gray" "#ff8888") :label label
             ;;                   :css `(:height "1.3em" :font-size 2em :text-align "center" :margin ,margin)))
             (otherwise (create-div control-buttons)))))))

(defun nanoktl2-preset-gui (id container &key (chan 5))
  (let ((midi-controller (or
                          (find-controller id)
                          (let ((ctlr (add-midi-controller 'nanoktl2-preset-midi
                                                           id
                                                           :midi-input cl-midictl::*midi-in1*
                                                           :midi-output cl-midictl::*midi-out1*
                                                           :chan chan)))
                            (setf (tr-rec ctlr) (make-bang (lambda () (handle-store-button-press ctlr)) 0))
                            ctlr)))
        (margin "0.2em"))
    (with-slots (fader s-buttons m-buttons r-buttons
                 button-labels
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
          for button-label across button-labels
          do (create-nk2-preset-button fader-panel button button-label margin))
        (loop
          for button across (m-buttons midi-controller)
          for button-label across (subseq button-labels 8)
          do (create-nk2-preset-button fader-panel button button-label margin))
        (loop
          for button across (r-buttons midi-controller)
          for bank-no from 1
          do (create-nk2-preset-bank-button fader-panel button (format nil "bank ~a" bank-no) margin))))))
