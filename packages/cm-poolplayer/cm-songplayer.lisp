;;; 
;;; cm-songplayer.lisp
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

(in-package :cm)

(eval-when (:compile-toplevel :load-toplevel)
  (defobject song (event)
      ((play-fn :initform nil :accessor play-fn)
       (duration :initform nil :accessor song-duration)
       (transp :initform nil :accessor song-transp)
       (preset  :initform nil :accessor song-preset)
       (color  :initform nil :accessor song-color)
       (args :initform nil :accessor song-args))
    (:parameters time play-fn duration transp preset args)
    (:event-streams)))

(defgeneric end-time (obj))

(defmethod end-time ((obj song))
  (with-slots (time duration) obj
    (+ time duration)))

(defmethod end-time ((obj midi))
  (with-slots (time duration) obj
    (+ time duration)))

(defmethod end-time ((obj sfz))
  (with-slots (time duration) obj
    (+ time duration)))

(defmethod end-time ((obj poolevt))
  (with-slots (lsample keynum start end stretch time) obj
    (with-slots (buffer) lsample
      (with-slots (buffer-frames buffer-sample-rate) buffer
        (let* ((buffer-dur (/ buffer-frames buffer-sample-rate))
               (start (min 0 start))
               (end (if (zerop end) buffer-dur (min end buffer-dur)))
               (stretch (ou:ct->fr (* 100 (- keynum (incudine::lsample-keynum lsample))))))
          (* stretch (- end start)))))))




;;; (end-time (new sfz :time 10 :duration 3 :preset :fl-nonvib))

(defun function-name (fn-obj)
  "return lowercase function name as string from a function object.

(function-name #'expt) -> \"expt\"

CAVEAT: This is working in sbcl, but it is unclear whether it'll be
the same in other Cl implementations as this is not standarized! The
code here could be extended for other implementations using pragmas."
  (cl-ppcre:regex-replace "#<function (.\+)>$" (format nil "~(~S~)" fn-obj) "\\1"))

;;; (function-name #'expt) -> "expt"



(defun serialize-events (score)
  (mapcar (lambda (obj)
            (with-slots (cl-poolplayer::ref
                         cl-poolplayer::duration
                         cl-poolplayer::x1
                         cl-poolplayer::y1
                         cl-poolplayer::color
                         cl-poolplayer::args
                         )
                obj
              (new song :play-fn (symbol-function cl-poolplayer::ref)
                :duration cl-poolplayer::duration
                :time cl-poolplayer::x1
                :transp cl-poolplayer::y1
                :color cl-poolplayer::color
                :args cl-poolplayer::args)))
          (cl-poolplayer::serialize-score score)))

(defparameter *color-lookup* nil)

;;; mapping of pd quo colors to #rgb

(setf *color-lookup*
      (let* ((assocs '((0 "#000000") (1 "#0000FF") (2 "#FF0000") (3 "#00FFFF") (4 "#00FF00")
                       (5 "#FFB400") (6 "#FF00FF") (7 "#FF97FF") (8 "#00008F") (9 "#0000B0")
                       (10 "#0000D1") (11 "#87CFFF") (12 "#008F00") (13 "#00B000") (14 "#00D100")
                       (15 "#008F8F") (16 "#00B0B0") (17 "#00D1D1") (18 "#8F0000") (19 "#B00000")
                       (20 "#D10000") (21 "#8F008F") (22 "#B000B0") (23 "#D100D1") (24 "#803000")
                       (25 "#A14000") (26 "#BF6100") (27 "#FF8080") (28 "#FFA1A1") (29 "#FFBFBF")
                       (30 "#FFE0E0") (31 "#FFD600") (32 "#FF334C") (33 "#CCCCCC") (34 "#999999")
                       (35 "#666666") (36 "#333333") (37 "#00E500") (38 "#007FFF") (39 "#CC7F19")
                       (40 "#8C19FF") (41 "#FFD5DA") (42 "#A5E5A5") (43 "#A0D0FF") (44 "#CCA570")
                       (45 "#CF9EFF") (46 "#CF9EFF")))
             (array (make-array (length assocs) :element-type 'string :initial-element "")))
        (loop for (idx color) in assocs do (setf (aref array idx) color))
        array))

(defmethod write-event ((obj song) (io incudine-stream) scoretime)
  (with-slots (play-fn time duration transp preset args) obj
    (sprout (apply play-fn :time (+ time scoretime) :duration duration :preset preset :transp transp args))))

(defmethod write-event ((obj song) (io svg-file) scoretime)
  (with-slots (play-fn time duration transp preset color args) obj
    (cond ((and (numberp (expand io)) (> (expand io) 0)) ;;; depth limit expansion
           (decf (expand io))                            ;;; decrement depth
           (mapcar (lambda (evt)
                     (write-event evt io (object-time obj)))
                   (apply play-fn :time (+ time scoretime) :duration duration :preset preset :transp transp args))
           (incf (expand io))) ;;; restore depth for processing of the next event in outer iteration
          ((and (expand io) (not (numberp (expand io))))
           (mapcar (lambda (evt)
                     (write-event evt io (object-time obj)))
                   (apply play-fn :time scoretime :duration duration :preset preset :transp transp args)))
          (t (let* ((x-scale (x-scale io))
                    (stroke-width 0.5)
                    (rgb-color (aref *color-lookup* color))
                    (line (let ((x1 (* x-scale scoretime))
                                (y1 (+ transp 60))
                                (dy 0)
                                (width
                                  (* x-scale
                                     duration))
                                (opacity 1))
                            (make-instance
                             'svg-ie::svg-line
                             :x1 (float x1 1.0) :y1 (float y1 1.0)
                             :x2 (float (+ x1 width) 1.0) :y2 (float (+ y1 dy) 1.0)
                             :stroke-width stroke-width
                             :stroke-opacity opacity
                             :opacity opacity
                             :stroke-color rgb-color 
                             ;; :fill-color rgb-color
                             :attributes (format nil ":type song :play-fn #'~a :preset ~a :args ~S"
                                                 (function-name play-fn) preset args)
                             :id (new-id io 'line-ids)))))
               (svg-file-insert-line line (new-id io 'line-ids) io))))))

#|
(defmethod write-event ((obj poolevt) (fil svg-file) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the elements slot of the
svg-file."
  (with-slots (lsample amp keynum dy start end stretch wwidth attack release pan snd-id adjust-stretch out1 out2) obj
    (with-slots (incudine::buffer incudine::play-fn incudine::keynum incudine::loopstart incudine::loopend) lsample
      (let* ((myid (incudine:buffer-id incudine::buffer))
             (filename (incudine:buffer-file incudine::buffer))
             (x-scale (x-scale fil))
             (stroke-width 0.5)
             (id (or snd-id (if (numberp myid) myid) 2))
             (color (chan->color id))
             (bufdur (float (/ (incudine::buffer-frames incudine::buffer)
                               (incudine:buffer-sample-rate incudine::buffer))
                            1.0))
             (region-end (if (zerop end) bufdur (min end bufdur)))
             (sample-region (- region-end start))
             (line (let ((x1 (* x-scale scoretime))
                         (y1 keynum)
                         (width
                           (* x-scale
                              stretch
                              sample-region))
                         (opacity (db->opacity amp)))
                     (make-instance
                      'svg-ie::svg-line
                      :x1 (float x1 1.0) :y1 (float y1 1.0)
                      :x2 (float (+ x1 width) 1.0) :y2 (float (+ y1 dy) 1.0)
                      :stroke-width stroke-width
                      :stroke-opacity opacity
                      :opacity opacity
                      :stroke-color color 
                      ;; :fill-color color
                      :attributes (format nil ":type poolevt :lsample ~A :lsample-keynum ~a :lsample-amp ~a :lsample-play-fn ~a :keynum ~a :amp ~a :start ~a :end ~a :stretch ~a :wwidth ~a :attack ~a :release ~a :pan ~a :out1 ~a :out2 ~a :loopstart ~a :loopend ~a :dy ~a :snd-id ~a :adjust-stretch ~a"
                                          filename
                                          incudine::keynum
                                          (incudine:lsample-amp lsample)
                                          (function-name incudine::play-fn)
                                          keynum
                                          amp
                                          start (if (= region-end bufdur) 0 region-end)
                                          stretch wwidth attack release pan out1 out2
                                          incudine::loopstart incudine::loopend
                                          dy
                                          id
                                          adjust-stretch)
                      :id (new-id fil 'line-ids)))))
        (svg-file-insert-line line (if (numberp myid) myid 2) fil)))))


(defmethod write-event ((obj midi) (fil svg-file) scoretime)
  "convert a midi object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (let* ((myid (midi-channel obj))
         (x-scale (x-scale fil))
         (stroke-width 0.5)
         (line (let ((x1 (* x-scale scoretime))
                     (y1 (* 1 (midi-keynum obj)))
                     (width (* x-scale (midi-duration obj)))
                     (color (chan->color myid))
                     (opacity (midi-amplitude obj)))
                 (make-instance 'svg-ie::svg-line :x1 (float x1) :y1 (float y1)
                                :x2 (float (+ x1 width)) :y2 (float y1)
                                :stroke-width stroke-width
                                :opacity opacity
                                :stroke-color color 
                                ;; :fill-color color
                                :id (new-id fil 'line-ids)))))
    (svg-file-insert-line line myid fil)))
|#

(export '(song play-fn preset transp args) :cm)
