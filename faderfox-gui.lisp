;;;
;;; faderfox-gui.lisp
;;;
;;; faderfox-gui besteht aus einer Gui Instanz (faderfox-gui) und
;;; einem Controller (faderfox-midi), der eine spezielle Klasse eines
;;; midicontrollers ist (definiert in cl-midictl).
;;; 
;;; faderfox-gui ist eine Klasse, die die Gui Instanz und den
;;; Hardware Controller zusammenfasst (faderfox-gui existiert nur der
;;; Vollständigkeit halber, falls der unwahrscheinliche Fall auftritt,
;;; dass man ein Gui ohne HardwareController verwenden möchte). Da es
;;; mehrere Gui Instanzen geben kann, die alle auf die selbe
;;; Controller Instanz bezogen sind, ist der Controller ein Slot der
;;; Gui Instanz von (faderfox-gui) und muss bei make-instance der
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
;;; Controller Klasse (faderfox-midi) geregelt und besteht nur darin,
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

(defclass faderfox-gui ()
  ((midi-controller :initarg :midi-controller :accessor midi-controller)
   (gui-parent :initarg :gui-parent :accessor gui-parent)
   (gui-container :initarg :gui-container :accessor gui-container)
   (gui-fader :initarg :gui-fader :accessor gui-fader)
   (gui-buttons :initarg :gui-buttons :accessor gui-buttons)
   (gui-ctl-panel :initarg :gui-ctl-panel :accessor gui-ctl-panel)
   (ctl-panel-vis :initform t :initarg :ctl-panel-vis :accessor ctl-panel-vis)))

(defmacro trigger-fn (slot)
  `(lambda (src) (trigger ,slot src)))

(defmacro define-buttons (gui-slot ctl-slot panel)
  `(setf ,gui-slot
         (coerce
          (v-collect (n 16)
                     (toggle
                      ,panel
                      :css gui-btn-css
                      :background '("gray" "#ff8888")
                      :values '("0" "127")
                      :label (1+ n)
                      :val-change-cb (lambda (v obj) (declare (ignore obj))
                                       (setf (val (aref (,ctl-slot midi-controller) n)) (read-from-string v)))))
          'vector)))

(defmethod initialize-instance :after ((instance faderfox-gui) &rest args)
  (declare (ignorable args))
  (with-slots (gui-parent gui-container
               gui-fader gui-buttons
               gui-ctl-panel
               midi-controller
               )
      instance
    (unless gui-parent (error "faderfox-gui initialized without parent supplied!"))
    (with-connection-cache (gui-parent)
      (setf gui-container (create-div gui-parent
                                      :css '(:display flex
                                             :flex-wrap wrap
;;;                                             :justify-content "space-between"
                                             :flex "0 0 auto"
                                             :margin-right 15px
                                             :padding-bottom 30px)))
      (let (fader-panel button-panel
            fader-subpanel button-subpanel
            (gui-btn-css '(:width 50px :height 15px :font-size 10px :margin 2px)))
        (setf fader-panel (create-div gui-container :css '(:width 220px
                                                           :height 80px
                                                           :display "flex"
                                                           :flex-direction "column"
                                                           :justify-content "flex-end"
                                                           :max-width 220px
                                                           :max-height 80px)))
        (setf fader-subpanel (create-div fader-panel :css '(:width "100%" :height 65px
                                                            :display "grid"
                                                            :grid-template-columns "1fr 1fr 1fr 1fr"
                                                            :grid-template-rows "1fr 1fr 1fr 1fr"
                                                            :gap 0px ;
                                                            :padding 10px
                                                            :justify-content "space-around"
                                                            :align-content "space-around")))
        (setf button-panel (create-div gui-container :css '(:width 220px
                                                            :height 80px
                                                            :display "flex"
                                                            :flex-direction "column"
                                                            :justify-content "flex-end"
                                                            :max-width 220px
                                                            :max-height 80px)))
        (setf button-subpanel (create-div button-panel :css '(:width "100%" :height 65px
                                                              :display "grid"
                                                              :grid-template-columns "1fr 1fr 1fr 1fr"
                                                              :grid-template-rows "1fr 1fr 1fr 1fr"
                                                              :gap 0px ;
                                                              :padding 10px
                                                              :justify-content "space-around"
                                                              :align-content "space-around")))
        (setf gui-fader
              (coerce
               (v-collect
                   (n 16)
                   (numbox
                    fader-subpanel
                    :min 0 :max 127 :size 10 :css '(:margin 2px)
                    :val-change-cb
                    (let ((n n))
                      (lambda (v obj)
                        (declare (ignore obj))
                        (let ((new-value (read-from-string v)))
                          (setf (val (aref (ff-faders midi-controller) n)) new-value))))))
               'vector))
        (define-buttons gui-buttons ff-buttons button-subpanel)))


#|

;;; set the ref-set-hooks in the model-slots of the midi-controller
    (dotimes (i 16) ;;; faders and knobs
      (with-slots (nk2-faders chan cc-nums) midi-controller
        (setf (ref-set-hook (aref nk2-faders i))
              (let ((i i))
                (lambda (val) 
                  (maphash (lambda (connection-id connection-hash)
                             (declare (ignore connection-id))
                             (let* ((f.orm-gui (gethash "f.orm-gui" connection-hash)))
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
                               (let* ((f.orm-gui (gethash "f.orm-gui" connection-hash)))
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
                             (let* ((f.orm-gui (gethash "f.orm-gui" connection-hash)))
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
                             (let* ((f.orm-gui (gethash "f.orm-gui" connection-hash)))
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
        (update-state instance)
|#
        ))

(defmethod update-state ((gui faderfox-gui))
  (with-slots (midi-controller gui-fader gui-m-buttons gui-s-buttons gui-r-buttons
               gui-rewind gui-ffwd gui-stop gui-play gui-rec)
      gui
    (with-slots (cc-nums cc-state chan) midi-controller
      (dotimes (i 16)
        (let ((elem (aref gui-fader i))
              (fader-value (val (aref cc-state i))))
          (setf (clog:value elem) fader-value)
          (setf (style elem :background-color)
                (if (= fader-value
                       (aref
                        (aref cl-midictl::*midi-cc-state* chan)
                        (aref cc-nums i)))
                    "#aaffaa" "#ffaaaa"))))
      (dotimes (i 8)
        (map '() (lambda (slot offs)
                   (let ((elem (aref slot i))
                         (value (val (aref cc-state (+ i offs)))))
                     (setf (clog:value elem) value)))
             (list gui-s-buttons gui-m-buttons gui-r-buttons)
             '(16 24 32)))
      (map '() (lambda (slot offs)
                   (let ((elem slot)
                         (value (val (aref cc-state offs))))
                     (setf (clog:value elem) value)))
             (list gui-rewind gui-ffwd gui-stop gui-play gui-rec)
             '(46 47 48 49 50)))))

(defgeneric show-ctl-panel (gui show)
  (:method ((gui faderfox-gui) show)
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
  (:method ((gui faderfox-gui))
    (with-slots (gui-ctl-panel ctl-panel-vis) gui
      (if ctl-panel-vis
          (progn
            (setf (style gui-ctl-panel "display") "none")
            (setf ctl-panel-vis nil))
          (progn
            (setf (style gui-ctl-panel "display") "flex")
            (setf ctl-panel-vis t))))))

;;; (add-midi-controller 'faderfox-gui :id :ff01 :chan 5)

;;; (find-controller :ff01)
