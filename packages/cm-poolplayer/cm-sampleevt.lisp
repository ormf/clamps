;;; 
;;; cm-sampleevt.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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
  (defobject sampleevt (event)
      ((lsample :initform nil :accessor sampleevt-lsample)
       (keynum :initform nil :accessor sampleevt-keynum)
       (transposable :initform t :accessor sampleevt-transposable)
       (amp :initform 0.0 :accessor sampleevt-amp)
;;;       (transp :initform 0.0 :accessor sampleevt-transp)
       (duration :initform 1 :accessor sampleevt-duration)
       (start :initform 0 :accessor sampleevt-start)
       (out :initform 0 :accessor sampleevt-out))
    (:parameters time lsample keynum amp duration start out)
    (:event-streams)))


(defmethod write-event ((obj sampleevt) (fil svg-file) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (with-slots (lsample keynum transposable amp duration start out) obj
    (let* ((buffer (of-incudine-dsps:lsample-buffer lsample))
           (x-scale (x-scale fil))
           (stroke-width 0.5)
           (id (incudine-bufs:buffer-id buffer))
           (color (chan->color id))
           (bufdur (float (/ (incudine::buffer-frames buffer)
                             (incudine:buffer-sample-rate buffer))
                          1.0))
           (dur (min duration (- bufdur start)))
           (line (let ((x1 (* x-scale scoretime))
                       (y1 (or keynum (float (of-incudine-dsps:lsample-keynum lsample) 1.0)))
                       (width (* x-scale dur))
                       (opacity (db->opacity amp)))
                   (make-instance
                    'svg-ie::svg-line
                    :x1 (float x1 1.0) :y1 y1
                    :x2 (float (+ x1 width) 1.0) :y2 y1
                    :stroke-width stroke-width
                    :stroke-opacity opacity
                    :opacity opacity
                    :stroke-color color 
                    ;; :fill-color color
                    :attributes (format nil ":type sampleevt :lsample ~A :transposable ~a :start ~a :lsample-amp ~a :lsample-keynum ~a :loopstart ~a :loopend ~a :oneshot ~a :out ~a"
                                        (of-incudine-dsps:lsample-pathname lsample)
                                        transposable
                                        start
                                        (of-incudine-dsps:lsample-amp lsample)
                                        (of-incudine-dsps:lsample-keynum lsample)
                                        (of-incudine-dsps:lsample-loopstart lsample)
                                        (of-incudine-dsps:lsample-loopend lsample)
                                        (of-incudine-dsps:lsample-oneshot lsample)
                                        out)
                    :id (new-id fil 'line-ids)))))
      (svg-file-insert-line line (if (numberp id) id 2) fil))))

(defmethod write-event ((obj sampleevt) (to incudine-stream) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (with-slots (lsample keynum amp duration start transposable out)
      obj
    (let* ((time (+ (rts-now) (if scoretime (* *rt-scale* scoretime) 0)))
           (keynum (if (and transposable keynum) keynum
                       (of-incudine-dsps:lsample-keynum lsample)))
           (amp (+ amp (of-incudine-dsps:lsample-amp lsample)))
;;;           (out (mod snd-id 8))
           )
      (at time #'of-incudine-dsps:play-lsample lsample keynum amp duration :startpos start))))

(defmethod write-event ((obj of-incudine-dsps:lsample) (to incudine-stream) scoretime)
  "play an lsample."
  (let* ((time (+ (rts-now) (if scoretime (* *rt-scale* scoretime) 0)))
         (buffer (oid:lsample-buffer obj))
         (dur (/ (incudine:buffer-size buffer) (incudine:buffer-sample-rate buffer))))
    (at time #'oid:play-lsample obj (oid:lsample-keynum obj) 0 dur)))

(svg-ie:add-svg-attr-props-to-quote :lsample)
;;;(svg-ie:add-svg-attr-props-to-quote :play-fn)

(defun svg->sampleevt (&rest args)
  "recreate a sampleevt from the :attributes property of the svg element."
;;;  (break "svg->poolevt: args: ~S" args)
  (ou:with-props (lsample lsample-keynum lsample-amp oneshot keynum
                          amplitude duration start loopstart loopend transposable
                          pan out1 out2)
      args
    (declare (ignorable start duration keynum transposable))
    (let* ((buf (incudine-bufs:find-buffer lsample))
           (buffer (if (consp buf) (first buf) buf))
           (new-lsample (of-incudine-dsps::make-lsample
                         :buffer buffer
                         :name (file-namestring (pathname lsample))
                         :keynum (float lsample-keynum 1.0d0)
                         :amp lsample-amp
                         :loopstart loopstart
                         :loopend loopend
                         :oneshot oneshot)))
      (apply #'make-instance 'sampleevt
             (list* :lsample new-lsample
                    :amp (opacity->db amplitude)
                    (ou:delete-props args :lsample :lsample-keynum :lsample-amp :play-fn
                                          :loopstart :loopend
                                          :amp :amplitude :channel))))))
;;; (setf *debug* t)


(add-svg-assoc-fns
 '((sampleevt . svg->sampleevt)))

;;; (new sampleevt)#i(sampleevt lsample nil keynum nil amp 0.0 duration 1 start 0 out 0)

(export '(svg->sampleevt sampleevt sampleevt-lsample sampleevt-keynum sampleevt-amp sampleevt-start sampleevt-out) 'cm)
