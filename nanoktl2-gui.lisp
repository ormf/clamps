;;;
;;; nanoktl2-gui.lisp
;;;
;;; nanoktl2-gui besteht aus einer Gui Instanz (nanoktl2-gui) und
;;; einem Controller (nanoktl2-midi), der eine spezielle Klasse eines
;;; midicontrollers ist (definiert in cl-midictl).
;;; 
;;; nanoktl2-gui ist eine Klasse, die die Gui Instanz und den
;;; Hardware Controller zusammenfasst (nanoktl2-gui existiert nur der
;;; Vollständigkeit halber, falls der unwahrscheinliche Fall auftritt,
;;; dass man ein Gui ohne HardwareController verwenden möchte). Da es
;;; mehrere Gui Instanzen geben kann, die alle auf die selbe
;;; Controller Instanz bezogen sind, ist der Controller ein Slot der
;;; Gui Instanz von (nanoktl2-gui) und muss bei make-instance der
;;; Gui Instanz übergeben werden (wird im on-new-window Code
;;; gemacht). Im Controller sind model-slots, deren set-cell
;;; Funktionen alle Slots im gui updaten.
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
;;; Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defclass nanoktl2-gui (clog-midi-controller)
  ((gui-parent :initarg :gui-parent :accessor gui-parent)
   (gui-container :initarg :gui-container :accessor gui-container)
   (gui-fader :initarg :gui-fader :accessor gui-fader)
   (gui-s-buttons :initarg :gui-s-buttons :accessor gui-s-buttons)
   (gui-m-buttons :initarg :gui-m-buttons :accessor gui-m-buttons)
   (gui-r-buttons :initarg :gui-r-buttons :accessor gui-r-buttons)
   (gui-ctl-panel :initarg :gui-ctl-panel :accessor gui-ctl-panel)
   (ctl-panel-vis :initform t :initarg :ctl-panel-vis :accessor ctl-panel-vis)
   (gui-track-left :initarg :gui-track-left :accessor gui-track-left)
   (gui-track-right :initarg :gui-track-right :accessor gui-track-right)
   (gui-cycle :initarg :gui-cycle :accessor gui-cycle)
   (gui-set-marker :initarg :gui-set-marker :accessor gui-set-marker)
   (gui-marker-left :initarg :gui-marker-left :accessor gui-marker-left)
   (gui-marker-right :initarg :gui-marker-right :accessor gui-marker-right)
   (gui-rewind :initarg :gui-rewind :accessor gui-rewind)
   (gui-ffwd :initarg :gui-ffwd :accessor gui-ffwd)
   (gui-stop :initarg :gui-stop :accessor gui-stop)
   (gui-play :initarg :gui-play :accessor gui-play)
   (gui-rec :initarg :gui-rec :accessor gui-rec)))

(defmacro trigger-fn (slot)
  `(lambda (src) (trigger ,slot src)))

(defmacro define-transport-button (gui-slot ctl-slot label panel)
  `(setf ,gui-slot
    (toggle ,panel :background '("gray" "#ff8888") :label ,label :values '(0 127) :css transport-btn-css
                             :val-change-cb (lambda (v obj)
                                              (declare (ignore obj))
                                              (let ((value (read-from-string v)))
                                                (setf (val (,ctl-slot midi-controller)) value))))))

(defmacro define-momentary-button (gui-slot ctl-slot label panel)
  `(setf ,gui-slot
         (bang ,panel :background '("gray" "#ff8888") :label ,label :css small-btn-css
                                :action-cb (trigger-fn (,ctl-slot midi-controller)))))

(defmacro define-button-row (gui-slot ctl-slot label panel)
  `(setf ,gui-slot
         (coerce
          (v-collect (n 8)
                     (toggle
                      ,panel
                      :css gui-btn-css
                      :background '("gray" "#ff8888")
                      :values '("0" "127")
                      :label ,label
                      :val-change-cb (lambda (v obj) (declare (ignore obj))
                                       (setf (val (aref (,ctl-slot midi-controller) n)) (read-from-string v)))))
          'vector)))

(defun flash-midi-out (stream cc-num chan)
  (osc-midi-write-short stream (+ chan 176) cc-num 127)
  (at (+ (now) 4410) #'osc-midi-write-short stream (+ chan 176) cc-num 0))

(defmethod initialize-instance :after ((instance nanoktl2-gui) &rest args)
  (declare (ignorable args))
  (with-slots (midi-controller connection-hash-key
               gui-parent gui-container
               gui-fader gui-s-buttons gui-m-buttons gui-r-buttons
               gui-track-left gui-track-right
               gui-cycle gui-set-marker gui-marker-left gui-marker-right
               gui-rewind gui-ffwd gui-stop gui-play gui-rec gui-ctl-panel
               )
      instance
    (unless gui-parent (error "nanoktl2-gui initialized without parent supplied!"))
    (with-connection-cache (gui-parent)
      (setf gui-container (create-div gui-parent
                                      :css '(:display flex
                                             :flex-wrap wrap
;;;                                             :justify-content "space-between"
                                             :flex "0 0 auto"
                                             :margin-right 15px
                                             :padding-bottom 30px)))
      (let (knob-fader-panel knob-panel fader-panel s-btn-panel m-btn-panel r-btn-panel
            gui-ctl-subpanel
            (small-btn-css '(:width 30px :height 10px :font-size 6px :margin 0px))
            (transport-btn-css '(:width 30px :height 15px :font-size 10px))
            (gui-btn-css '(:width 50px :height 15px :font-size 10px :margin 2px)))
        (setf gui-ctl-panel (create-div gui-container :css '(:width 180px
                                                             :height 103px
                                                             :display "flex"
                                                             :flex-direction "column"
                                                             :justify-content "flex-end"
                                                             :max-width 180px
                                                             :max-height 103px)))
        (setf gui-ctl-subpanel (create-div gui-ctl-panel :css '(:width "100%" :height 65px
                                                                :display "grid"
                                                                :grid-template-columns "1fr 1fr 1fr 1fr 1fr"
                                                                :grid-template-rows "1fr 1fr 1fr"
                                                                :gap 5px ;
                                                                :padding 10px
                                                                :justify-content "space-around"
                                                                :align-content "space-around")))
        (setf knob-fader-panel (create-div gui-container :css '(:line-height 0)))
        (setf knob-panel (create-div knob-fader-panel))
        (create-br knob-fader-panel)
        (setf fader-panel (create-div knob-fader-panel))
        (create-br knob-fader-panel)
        (setf s-btn-panel (create-div knob-fader-panel))
        (create-br knob-fader-panel)
        (setf m-btn-panel (create-div knob-fader-panel))
        (create-br knob-fader-panel)
        (setf r-btn-panel (create-div knob-fader-panel))        

        (define-momentary-button gui-track-left track-left "<" gui-ctl-subpanel)
        (define-momentary-button gui-track-right track-right ">" gui-ctl-subpanel)
        (dotimes (n 3) (create-div gui-ctl-subpanel))
        (define-momentary-button gui-cycle cycle "cycle" gui-ctl-subpanel)
        (create-div gui-ctl-subpanel)
        (define-momentary-button gui-set-marker set-marker "set" gui-ctl-subpanel)
        (define-momentary-button gui-marker-left marker-left "<" gui-ctl-subpanel)
        (define-momentary-button gui-marker-right marker-right ">" gui-ctl-subpanel)

        (define-transport-button gui-rewind tr-rewind "1" gui-ctl-subpanel)
        (define-transport-button gui-ffwd tr-ffwd "2" gui-ctl-subpanel)
        (define-transport-button gui-stop tr-stop "3" gui-ctl-subpanel)
        (define-transport-button gui-play tr-play "4" gui-ctl-subpanel)
        (define-transport-button gui-rec tr-rec "5" gui-ctl-subpanel)

        (setf gui-fader
              (coerce
               (loop for panel in (list knob-panel fader-panel)
                     for offs in '(0 8)
                     append (v-collect
                                (n 8)
                                (numbox
                                 panel
                                 :min 0 :max 127 :size 10 :css '(:margin 2px)
                                 :val-change-cb
                                 (let ((n (+ n offs)))
                                   (lambda (v obj)
                                     (let ((new-value (read-from-string v)))
                                       (setf (val (aref (nk2-faders midi-controller) n)) new-value)
;;; nk2-fader-update-fns are functions to compare incoming
;;; midi-cc-values from the hardware midi-controller against the current
;;; value in the gui. As soon as the update-fn returns t, the
;;; hardware fader is "caught" and the background in the gui is set
;;; to green. This is implemented in the handle-midi-in method of the
;;; midi-controller side of the code (in nanoktl2.lisp of
;;; cl-midictl). Here these functions and the background colors are
;;; set up in response to a change in the gui.
;;;
;;; IMPORTANT NOTE: The physical state of the hardware controller is
;;; maintained in cl-midictl:*midi-cc-state* and *NOT* in the cc-state
;;; of the midi-controller instance. That state is always in sync with
;;; the gui (using model-slots and synchronizing with the gui and the
;;; hardware controller via its ref-set-hooks defined below).
                                       (setf (aref (nk2-fader-update-fns midi-controller) n)
                                             (let ((hw-val (aref
                                                            (aref *midi-cc-state*
                                                                  (chan midi-controller))
                                                            (aref (cc-nums midi-controller) n))))
                                               (cond
                                                 ((> new-value hw-val)
                                                  (setf (style obj :background-color) "#ffaaaa")
                                                  #'>=)
                                                 ((< new-value hw-val)
                                                  (setf (style obj :background-color) "#ffaaaa")
                                                  #'<=)
                                                 (t (setf (style obj :background-color) "#aaffaa")
                                                    nil))))))))))
               'vector))
        (define-button-row gui-s-buttons s-buttons "S" s-btn-panel)
        (define-button-row gui-m-buttons m-buttons "M" m-btn-panel)
        (define-button-row gui-r-buttons r-buttons "R" r-btn-panel)))

    (setf (slot-value midi-controller 'echo) nil)
;;; set the ref-set-hooks in the model-slots of the midi-controller
    (dotimes (i 16) ;;; faders and knobs
      (with-slots (nk2-faders chan cc-nums) midi-controller
        (setf (ref-set-hook (aref nk2-faders i))
              (let ((i i))
                (lambda (val) 
                  (maphash (lambda (connection-id connection-hash)
                             (declare (ignore connection-id))
                             (let* ((f.orm-gui (gethash connection-hash-key connection-hash)))
                               (when f.orm-gui
                                 (let ((elem (aref (gui-fader f.orm-gui) i)))
                                   (setf (clog:value elem) val)
                                   (setf (style elem :background-color)
                                         (if (= val
                                                (aref
                                                 (aref cl-midictl::*midi-cc-state* chan)
                                                 (aref cc-nums i)))
                                             "#aaffaa" "#ffaaaa"))))))
                           clog-connection::*connection-data*))))))
    (dolist (syms '((s-buttons gui-s-buttons 16)  ;;; buttons next to faders
                    (m-buttons gui-m-buttons 24)
                    (r-buttons gui-r-buttons 32)))
      (destructuring-bind (nk2-slot gui-slot cc-map-offs) syms
        (dotimes (i 8)
          (setf (ref-set-hook (aref (slot-value midi-controller nk2-slot) i))
                (let* ((i i)
                       (chan (chan midi-controller))
                       (cc-num (elt (cc-nums midi-controller) (+ i cc-map-offs))))
                  (lambda (val) 
                    (osc-midi-write-short *midi-out1* (+ chan 176) cc-num val)
                    (maphash (lambda (connection-id connection-hash)
                               (declare (ignore connection-id))
                               (let* ((f.orm-gui (gethash connection-hash-key connection-hash)))
                                 (when f.orm-gui
                                   (let ((elem (aref (slot-value f.orm-gui gui-slot) i)))
                                     (setf (clog:attribute elem "data-val") val)))))
                             clog-connection::*connection-data*)))))))
    (dolist (syms '((tr-rewind gui-rewind 46) ;;; transport buttons
                    (tr-ffwd gui-ffwd 47)
                    (tr-stop gui-stop 48)
                    (tr-play gui-play 49)
                    (tr-rec gui-rec 50)))
      (destructuring-bind (nk2-slot gui-slot cc-map-offs) syms ;;; assigning transport button hooks
        (setf (ref-set-hook (slot-value midi-controller nk2-slot))
              (let* ((chan (chan midi-controller))
                     (cc-num (elt (cc-nums midi-controller) cc-map-offs)))
                (lambda (val)
                  (incudine.util:msg :info "val: ~a, chan: ~a, cc-num: ~a" val chan cc-num)
                  (osc-midi-write-short (midi-output midi-controller) (+ chan 176) cc-num (if (zerop val) 0 127))
                  (maphash (lambda (connection-id connection-hash)
                             (declare (ignore connection-id))
                             (let* ((f.orm-gui (gethash connection-hash-key connection-hash)))
                               (when f.orm-gui
                                 (let ((elem (slot-value f.orm-gui gui-slot)))
                                   (setf (clog:attribute elem "data-val") val)))))
                           clog-connection::*connection-data*))))))

    (dolist (syms '((track-left gui-track-left 40)
                    (track-right gui-track-right 41)
                    (cycle gui-cycle 42)
                    (set-marker gui-set-marker 43)
                    (marker-left gui-marker-left 44)
                    (marker-right gui-marker-right 45)))
      (destructuring-bind (nk2-slot gui-slot cc-map-offs) syms ;;; assigning click event handlers
        (setf (slot-value (slot-value midi-controller nk2-slot) 'cellctl:action-fn)
              (let* ((chan (chan midi-controller))
                     (cc-num (elt (cc-nums midi-controller) cc-map-offs)))
                (lambda (obj)
                  (incudine.util:msg :info "chan: ~a, cc-num: ~a" chan cc-num)
                  (flash-midi-out (midi-output midi-controller) cc-num chan)
                  (maphash (lambda (connection-id connection-hash)
                             (declare (ignore connection-id))
                             (let* ((f.orm-gui (gethash connection-hash-key connection-hash)))
                               (when f.orm-gui
                                 (let ((elem (slot-value f.orm-gui gui-slot)))
                                   (if (not (eql obj elem)) (flash elem))))))
                           clog-connection::*connection-data*))))))
;;; setup of F1/F2 switches to show/hide ctl-panel
    (let ((gui-ctl-panel-id (html-id gui-ctl-panel)))
      (clog:js-execute
       gui-parent
       (format nil "document.onkeydown = function (event) {
  if (event.which == 112 || event.keyCode == 112) {
   document.getElementById('~a').style.display = \"none\";
  }
  if (event.which == 113 || event.keyCode == 113) {
   document.getElementById('~a').style.display = \"flex\";
  }
};
"  gui-ctl-panel-id gui-ctl-panel-id)))
    (update-state midi-controller)))

(defgeneric show-ctl-panel (gui show)
  (:method ((gui nanoktl2-gui) show)
    (with-slots (gui-ctl-panel ctl-panel-vis) gui
      (if show
          (progn
            (setf (style gui-ctl-panel "display") "flex")
            (setf ctl-panel-vis t)
;;;            (setf (style gui-ctl-panel "max-width") "160px")
            )
          (progn
            (setf (style gui-ctl-panel "display") "none")
            (setf ctl-panel-vis nil)
;;;            (setf (style gui-ctl-panel "max-width") "0")
            )))))

(defgeneric toggle-ctl-panel-vis (gui)
  (:method ((gui nanoktl2-gui))
    (with-slots (gui-ctl-panel ctl-panel-vis) gui
      (if ctl-panel-vis
          (progn
            (setf (style gui-ctl-panel "display") "none")
            (setf ctl-panel-vis nil))
          (progn
            (setf (style gui-ctl-panel "display") "flex")
            (setf ctl-panel-vis t))))))

;;; (add-midi-controller 'nanoktl2-gui :id :nk2 :chan 5)

;;; (find-controller :nk2)
