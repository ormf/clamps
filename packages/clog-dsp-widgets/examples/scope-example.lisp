;;; 
;;; scope-example.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :clamps)

(progn
  (defparameter amp (make-ref 0))
  (defparameter sine-freq (make-ref 440))
  (defparameter scope-freq (make-ref 10))
  (defparameter color (make-ref "#333333"))
  (defparameter buffer (make-ref (incudine:make-buffer 300)))
  (defparameter amp-db
    (make-computed
     (lambda () (ou:clip (ou:amp->db (get-val amp)) -40 0))
     (lambda (val) (%set-val amp (ou:clip (if (<= val -40) 0 (ou:db->amp val)) 0 1)))))
  (defparameter *scope* nil)
  (defparameter unwatch nil)


  (defun clamps-gui (body)
    "On-new-window handler."
    (setf (clog:title (clog:html-document body)) "Gui Test")
    (let ((collection (create-collection body "1/2")))
      (create-o-knob collection (bind-refs-to-attrs amp "value") :min 0 :max 1 :step 0.01)
      (create-o-knob collection (bind-refs-to-attrs amp-db "value") :min -40 :max 0 :step 1 :unit "dB" :precision 0)
      (create-o-knob collection (bind-refs-to-attrs sine-freq "value") :min 10 :max 4000 :step 1
                     :unit "Hz"
                     :css '(:width "12em"))
      (create-o-scope collection (bind-refs-to-attrs color "color") :buffer buffer)
      (create-o-knob collection (bind-refs-to-attrs scope-freq "value") :min 6 :max 10 :step 1
                     :unit "Hz"
                     :css '(:width "12em"))))
    
  (clog:set-on-new-window #'clamps-gui))

(in-package :incudine)

(dsp! simple (freq amp (out channel-number))
  (:defaults 440 0.1 0)
  (with ((ph (phasor* (lag freq 0.01) 0)))
    (maybe-expand ph)
    (foreach-frame
      (incf (audio-out out current-frame)
            (* (lag amp 0.5) (sin (* +twopi+ (frame-ref ph current-frame))))))))

(progn
  (free 18)
  (free 19)
  (simple (cl-refs:get-val clamps::sine-freq) (cl-refs:get-val clamps::amp) 0 :head 200 :id 18)
  (out-scope 0 10 clamps::buffer :id 19 :tail 200 :after 18)
  (cl-refs:watch (lambda () (set-control 19 :freq (cl-refs:get-val clamps::scope-freq))))
  (cl-refs:watch (lambda () (set-control 18 :freq (cl-refs:get-val clamps::sine-freq))))
  (cl-refs:watch (lambda () (set-control 18 :amp (cl-refs:get-val clamps::amp)))))

