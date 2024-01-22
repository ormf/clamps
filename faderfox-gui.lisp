;;;
;;; faderfox-gui.lisp
;;;
;;; faderfox-gui besteht aus einer Gui Instanz (faderfox-gui) und
;;; einem Controller (faderfox-midi), der eine spezielle Klasse eines
;;; midicontrollers ist (definiert in cl-midictl).
;;; 
;;; faderfox-gui ist eine Funktion, die die html Objekte für jede Gui
;;; Instanz generiert und mit dem Hardware Controller
;;; zusammenfasst. Da es mehrere Gui Instanzen geben kann, die alle
;;; auf die selbe Controller Instanz bezogen sind, muss der Controller
;;; außerhalb der faderfox-gui Funktion existieren (die Controller
;;; Instanz wird mit add-midi-controller erzeugt). Im Controller sind
;;; ref-objects, deren set-val Funktionen die entsprechenden Slots in
;;; allen guis updaten (durch bind-ref-to-attr) und dem gewünschten
;;; Verhalten der MidiController entsprechende Aktionen triggern
;;; (durch watch Funktionen, die in der intialize-instance :after
;;; Methode der MidiController Klassen eingerichtet werden).
;;;
;;; Um mangels Motorfadern/Endlosreglern bei Controllern, wie dem
;;; NanoKontrol2, Werte "fangen" zu können bzw. durch Skalieren
;;; anzugleichen, um Wertesprünge zu vermeiden, gibt es
;;; cl-midictl:*midi-cc-state*, der immer den aktuellen Stand der
;;; HardwareFader/knobs enthält (der responder dafür wird automatisch
;;; gestartet) und der abhängig vom Faderwert der Klasse dann
;;; interpoliert/gefangen werden kann. *midi-cc-state* enthält keine
;;; ref-objects, da die Werte nur gesetzt werden, wenn der Fader
;;; bewegt wird, ansonsten aber nur gelesen werden müssen, wenn das
;;; gui mit den HardwareControllern verglichen werden soll.
;;;
;;; Das konkrete Verhalten des Controllers ist in handle-midi-in der
;;; Controller Klasse (faderfox-midi) geregelt und besteht nur darin,
;;; den Wert des jeweiligen ref-objects zu aktualisieren. Das
;;; Verhalten wird dann in dem ref-object geregelt (siehe Erklärung
;;; oben).
;;;
;;; **********************************************************************
;;; Copyright (c) 2023/24 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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


;;;                   (osc-midi-write-short midi-output (+ chan 176) (aref cc-nums i) (round val))


(in-package :cl-midictl)

(defclass faderfox-midi-f.orm (faderfox-midi)
  ((curr-player :initform 0 :type (integer 0 3) :accessor curr-player)))

(defmethod initialize-instance :after ((controller faderfox-midi-f.orm) &rest args)
  (declare (ignore args))
  (with-slots (note-state curr-player unwatch) controller
    (dotimes (player-idx 4)
      (push
       (watch ;;; handle player switch (radio behaviour of top 4 buttons)
        (let* ((idx player-idx)
               (button (aref note-state idx)))
          (lambda ()
            (when (= (get-val button) 127)
              (set-val (aref note-state curr-player) 0)
              (setf curr-player idx)))))
       unwatch))))

(export '(curr-player faderfox-midi-f.orm) 'cl-midictl)

#|

(defun set-player-enable (idx state)
  (with-slots (cl-poolplayer::playing cl-poolplayer::play-fn) (aref *players* idx)
    (setf cl-poolplayer::playing (> state 0))
    (if (> state 0)
        (pmde-01-lve nil :player idx :preset idx))))

|#

(in-package :clog-midi-controller)

(defun faderfox-gui (id gui-parent)
  (let ((midi-controller (find-controller id)))
    (with-slots (cc-state note-state) midi-controller
      (let* ((gui-container (create-div gui-parent
                                        :class "nk2-panel"
                                        :css '(:width "80%")))
             (fader-panel (create-div gui-container :css '(:display "flex"
                                                           :flex-direction "column")))
             (fader-subpanel (create-div fader-panel
                                         :css '(:display "grid"
                                                :grid-template-columns "repeat(4, minmax(3em, 0.37fr))")))
             (button-panel (create-div gui-container
                                       :css '(:width "40%"
                                              :display "grid"
                                              :grid-template-columns "repeat(4, minmax(3em, 0.6fr))"))))
        (dotimes (idx 16)
          (create-o-numbox
           fader-subpanel
           (bind-ref-to-attr (aref cc-state idx) "value")
           0 127 :precision 0 :css '(:text-align "center" :background "#ccc"
                                     :font-size "2em" :height "1.3em" :margin 2px))
          (create-o-toggle
           button-panel
           (bind-ref-to-attr (aref note-state idx) "value")
           :background '("#888" "#f88")
           :css '(:font-size "2em" :margin 2px )
           :values '(0 127)  ))))))
