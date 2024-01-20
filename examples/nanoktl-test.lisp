;;; 
;;; nanoktl-test.lisp
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
(ql:quickload :cl-midictl)
(ql:quickload :clog-midi-controller)

(in-package :clog-widgets)

(use-package :cl-midictl)

(handle-midi-in)
(typep 8 '(integer 0 10))

(setf (curr-bank (find-controller :nk2)) 30)

(setf *midi-in1* (jackmidi:open :direction :input
                                   :port-name "midi_in_1"))

(setf *midi-out1* (jackmidi:open :direction :output
                                    :port-name "midi_out_1"))



#|
(jackmidi:close *midi-in1*)
(jackmidi:close *midi-out1*)
|#

(start-midi-receive *midi-in1*)

(incudine:remove-all-responders *midi-in1*)

(setf (incudine.util:logger-level) :info)

(remove-midi-controller :nk2)

(set-val (aref (r-buttons (find-controller :nk2)) 2) 127)



(select-preset-bank (find-controller :nk2) 3)

(handle-midi-in)

(add-midi-controller
 'nanoktl2-preset-midi
 :midi-input *midi-in1* :midi-output *midi-out1* :id :nk2 :chan 5)

cl-midictl::*midi-controllers*

;;; (make-instance 'nanoktl2-preset-midi)

(find-controller :nk2)

(setf (incudine.util:logger-level) :info)

(update-state (find-controller :nk2))

(in-package :clog-widgets)

;;; gui

(defun create-nk2-numbox (parent midi-controller i margin)
  (let* ((min 0) (max 127)
         (precision 2)
         (css `(:background "#ddd" :text-align "center"  :font-size "2em" :height "1.3em" :margin ,margin))
         (binding (bind-ref-to-attr (aref (nk2-faders midi-controller) i) "value"))
         (var (b-ref binding))
         (attr (b-attr binding))
         (element (create-child
                   parent
                   (format nil "<input is=\"o-numbox\" min=\"~a\" max=\"~a\" value=\"~a\" precision=\"~a\" ~@[~a~]>"
                           min max (get-val (b-ref binding)) precision
                           (format-style css))))
         (unwatch
           (with-slots (cc-state cc-nums chan nk2-fader-modes) midi-controller
             (watch (lambda ()
                      (let ((fader-value (get-val (aref cc-state i)))
                            (hw-value (aref
                                       (aref cl-midictl::*midi-cc-state* chan)
                                       (aref cc-nums i))))
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
  (let* ((binding (bind-ref-to-attr button "value"))
         (label-off-binding (bind-ref-to-attr (elt (button-labels midi-controller) idx) "label-off"))
         (label-on-binding (bind-ref-to-attr (elt (button-labels midi-controller) idx) "label-on"))
         (css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.3em" :margin ,margin))
         (var (b-ref binding))
         (attr (b-attr binding))
         (label (b-ref label-off-binding))
         (element (create-child
                   parent
                   (format nil "<o-toggle ~{~@[~a ~]~}~@[~a~]>~@[~a~]</o-toggle>"
                           (list
                            (opt-format-attr "value" (get-val var))
                            (opt-format-attr "label-off" (get-val label))
                            (opt-format-attr "label-on" (get-val label))
                            (opt-format-attr "background-off" "#888")
                            (opt-format-attr "background-on" "#f88")
                            (opt-format-attr "color-off" "black")
                            (opt-format-attr "color-on" "black")
                            (opt-format-attr "value-off" 0)
                            (opt-format-attr "value-on" 1))
                           (format-style css)
                           label))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (push element (b-elist label-off-binding)) ;;; register the browser page's html elem for value updates.
    (push element (b-elist label-on-binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
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
                              (%set-val var (read-from-string (gethash attr data))))))))
    element))

(create-div gui-container :css '(:width 180px
                                                             :height 103px
                                                             :display "flex"
                                                             :flex-direction "column"
                                                             :justify-content "flex-end"
                                                             :max-width 180px
                                                             :max-height 103px))

        (define-momentary-button gui-track-left track-left "<" gui-ctl-subpanel)
        (define-momentary-button gui-track-right track-right ">" gui-ctl-subpanel)
        (dotimes (n 3) (create-div gui-ctl-subpanel))

;;; (nanoktl2-preset-midi)

(defun nanoktl2-gui (id container)
  (let ((midi-controller (or
                          (find-controller id)
                          (add-midi-controller 'nanoktl2-preset-midi :midi-input *midi-in1* :midi-output *midi-out1* :id id :chan 5)
                          ))
        (margin "0.2em"))
    (with-slots (fader s-buttons m-buttons r-buttons
                 track-left track-right
                 cycle set-marker marker-left marker-right
                 tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        midi-controller

      (let* (control-panel fader-panel)
        (setf control-panel (create-grid container "nk2-ctl" "5/15"))
        (create-br container)
        (setf fader-panel (create-grid container "nk2-fader" "8/40"))
        (loop
          for type in '(bang bang nil nil nil
                        bang nil bang bang bang
                        toggle toggle toggle toggle toggle)
          for label in '("<" ">" nil nil nil
                         "cycle" nil "set" "<" ">"
                         "player 1" "player 2" "player 3" "player 4" "store")
          for button in (list
                         track-left track-right nil nil nil
                         cycle nil set-marker marker-left marker-right
                         tr-rewind tr-ffwd tr-stop tr-play tr-rec)
          do (case type
               (bang
                (create-o-bang control-panel (bind-ref-to-attr button "bang")
                               :background '("gray" "#ff8888") :label label
                               :css `(:height "1.3em" :font-size 1.5em :text-align "center" :margin ,margin)))
               (toggle
                (create-o-toggle control-panel (bind-ref-to-attr button "value")
                                 :background '("gray" "#ff8888") :label label
                                 :css `(:height "1.3em" :font-size 2em :text-align "center" :margin ,margin)))
               (otherwise (create-div control-panel))))
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
                              :css `(:text-align "center" :user-select "none" :font-size "2em" :height "1.4em" :margin ,margin)
                              :background '("#888" "#f88")
                              :label (format nil "bank ~a" bank-no)))))))

#|
(defparameter *my-watch* (watch (lambda () (format t "~&moved: ~a~%" (get-val (aref (nk2-faders (find-controller :nk2)) 8))))))
(funcall *my-watch*)
|#

(set-val (elt (button-labels (find-controller :nk2)) 1) "11")

(setf (attribute (first (b-elist (gethash "ref1585-label-off" *bindings*))) "label-off") 100)

             (setf (attribute obj attr) val)
(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Gui Test")
  (nanoktl2-gui :nk2 body))

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
              :port 8081
              :static-root (merge-pathnames "www/" (asdf:system-source-directory :clog-widgets))
              :boot-file "/start.html")
  ;; Open a browser to http://127.0.0.1:8080 - the default for CLOG apps
  (open-browser))

;;; (start) should start a webserver with some gui widgets that are
;;; connected

(start)
