;;;
;;; nanoktl2-gui.lisp
;;;
;;; nanoktl2-gui besteht aus einer Gui Instanz (nanoktl2-gui) und
;;; einem Controller (nanoktl2-midi bzw. nanoktl2-preset-midi), der
;;; eine spezielle Klasse eines midicontrollers ist (definiert in
;;; cl-midictl).
;;; 
;;; nanoktl2-gui ist eine Funktion, die aufgerufen wird, wenn eine
;;; neue Webseite mit einem NanoKONTROL2 gui Element erzeugt werden
;;; soll. Da es mehrere Webseiten geben kann, die alle auf die selbe
;;; Controller Instanz bezogen sind, ist die Midicontroller Instanz
;;; eine lokale Variable in nanoktl2-gui, dessen id nanoktl2-gui
;;; übergeben wird: Wenn ein MidiController mit dieser id noch nicht
;;; existiert, wird eine nanoktl2-midi Instanz im gui instantiiert,
;;; ansonsten wird die existierende MidiController Instanz
;;; verwendet. Das bedeutet, dass im Falle, dass eine
;;; nanoktl2-preset-midi Instanz verwendet werden soll, diese Instanz
;;; schon mit #'add-midi-controller erzeugt worden sein muss, *bevor*
;;; das erste gui im Browser geöffnet wird.
;;;
;;; Um mangels Motorfadern/Endlosreglern des NanoKontrol2 Werte
;;; "fangen zu können", um Wertesprünge zu vermeiden, gibt es
;;; cl-midictl:*midi-cc-state*, der immer den aktuellen Stand der
;;; HardwareFader/knobs enthält (der responder dafür wird automatisch
;;; gestartet). *midi-cc-state* enthält keine model-slots, da die
;;; Werte einfach nur gesetzt werden, wenn der Fader bewegt wird,
;;; ansonsten aber nur gelesen werden müssen, wenn das gui mit den
;;; HardwareControllern verglichen werden soll.
;;;
;;; Das konkrete Verhalten des Controllers ist in handle-midi-in der
;;; Controller Klasse (nanoktl2-midi) geregelt und besteht nur darin,
;;; den Wert des jeweiligen model-slots zu aktualisieren (wenn der
;;; Wert "gefangen" ist).
;;;
;;; Bei Instantiierung des Gui Instanz (initialize-instance :after)
;;; werden an die model-slots der Controller Instanz set-cell-hooks
;;; gebunden, die alle existierenden Gui Referenzen updaten.
;;;
;;; **********************************************************************
;;; Copyright (c) 2023-25 Orm Finnendahl
;;; <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defun create-control-panel (parent midi-controller margin)
  (create-div parent :style "height: 3.05em;font-size: 3em" :content "nanoKontrol2")
  (let ((control-buttons (create-grid parent "nk2-ctl" "3/15")))
    (with-slots (button-labels track-left track-right
                 cycle set-marker marker-left marker-right
                 tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        midi-controller
      (loop
        for label in '("<" ">" nil nil nil
                       "cycle" nil "set" "<" ">"
                       "<<" ">>" "stop" "play" "rec")
        with label-idx = 23
        for button in (list
                       track-left track-right nil nil nil
                       cycle nil set-marker marker-left marker-right
                       tr-rewind tr-ffwd tr-stop tr-play tr-rec)
        do (progn
             (when label (set-val (aref button-labels (incf label-idx)) label))
             (if button
                 (create-o-bang control-buttons (bind-refs-to-attrs
                                                 button "bang"
                                                 button "highlight"
                                                 (aref button-labels label-idx) "label-off"
                                                 (aref button-labels label-idx) "label-on")
                                :background '("gray" "#ff8888") :label (get-val (aref button-labels label-idx))
                                :css `(:height "1.3em" :font-size "1.5em" :text-align "center" :margin ,margin))
                 (create-div control-buttons)))))))

(defun create-nk2-numbox (parent midi-controller i margin)
  "Return a numbox for nk2 knobs and faders."
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
             (watch (lambda () ;;; background styling of numbox according to hw state (synced/notsynced/scale(.
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
                                    (if (= fader-value hw-value) "#aaffaa" "#ffaaaa"))))))))))
    (push element (b-elist binding)) ;;; register the browser page's html elem for value updates.
    (set-on-data element ;;; react to changes in the browser page
                 (lambda (obj data)
		   (declare (ignore obj))
                   (incudine.util:msg :debug "~&~%clog event from ~a: ~a~%" element
                                      (or (if (gethash "close" data) "close")
                                          (gethash attr data)))
                   (if (gethash "close" data)
                       (progn
                         (funcall unwatch)
                         (b-unregister element binding))
                       (let ((*refs-seen* (list (list element (gethash attr data)))))
                         (%set-val var (gethash attr data))))))
    element))

(defun create-nk2-button (parent midi-controller button idx margin)
;;;  (break "idx: ~a" idx)
  (create-o-bang parent (bind-refs-to-attrs
                         button "bang"
                         button "highlight"
                         (aref (button-labels midi-controller) idx) "label-off"
                         (aref (button-labels midi-controller) idx) "label-on")
                 :background '("#888" "#f88")
                 :css `(:margin ,margin)))

(defun nanoktl2-gui (id container &key (chan 6))
  "Function called when a new browser page containing a nanoktl2-gui is
opened. The midicontroller is the central (SPOT) instance, containing
the state of all buttons and faders and handling MIDI IO in case a
hardware controller is attached. nanoktl2-gui defines the gui layout
and establishes the bidirectional bindings between the slots of the
midicontroller instance and the HTML attributes of the corresponding
gui elements."
  (let ((midi-controller
          (or
           (find-controller id)
           (add-midi-controller 'cl-midictl::nanoktl2-midi
                                id
                                :midi-input cm:*midi-in1*
                                :midi-output (cm:ensure-jackmidi cm:*midi-out1*)
                                :chan chan)))
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
;;;        (install-ctl-panel-key-switch nk2-panel (clog:html-id control-panel))
         (create-control-panel control-panel midi-controller margin)
         (create-br container)
        (setf fader-panel (create-grid nk2-panel "nk2-fader" "8/40"))
        (dotimes (idx 16)
          (create-nk2-numbox fader-panel midi-controller idx margin))  
        (loop
          for button across (s-buttons midi-controller)
          for idx from 0
          do (create-nk2-button fader-panel midi-controller button idx margin))
        (loop
          for button across (m-buttons midi-controller)
          for idx from 8
          do (create-nk2-button fader-panel midi-controller button idx margin))
        (loop
          for button across (r-buttons midi-controller)
          for idx from 16
          do (create-nk2-button fader-panel midi-controller button idx margin))))))

#|

;;; example:

(defun call-nanoktl2-gui (body)
  "helper function calling the current definition of #'nanoktl2-gui at the
time of URL reload."
  (nanoktl2-gui :nk2-01 body))

(add-midi-controller 'nanoktl2-midi :nk2-01)

(clog:set-on-new-window #'call-nanoktl2-gui :path "/nanoktl2-gui")

(remove-midi-controller :nk2-01)

;;; Code to change momentary buttons to toggles:

(defparameter *unwatch-triggers* nil)

(progn
  (map nil #'funcall *unwatch-triggers*)
  (setf *unwatch-triggers* nil)
  (setf *unwatch-triggers*
        (let ((controller (find-controller :nk2-01)))
          (with-slots (cc-state) controller
            (cons (toggle-ref-watch (aref cc-state (1- (length cc-state))) 3)
                  (mapcar #'toggle-ref-watch (coerce (subseq cc-state  16 (1- (length cc-state))) 'list)))))))



|#
