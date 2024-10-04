;;; 
;;; cm-poolevt.lisp
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

(in-package :cm)

(eval-when (:compile-toplevel :load-toplevel)
  (defobject poolevt (event)
      ((lsample :initform nil :accessor poolevt-lsample)
       (keynum :initform nil :accessor poolevt-keynum)
       (amp :initform 0 :accessor poolevt-amp)
;;;       (transp :initform 0.0 :accessor poolevt-transp);;; removed in favor of keynum (+ transp lsample-keynum)
       (dy :initform 0.0 :accessor poolevt-dy)
       (start :initform 0 :accessor poolevt-start)
       (end :initform 0 :accessor poolevt-end)
       (stretch :initform 1.0 :accessor poolevt-stretch)
       (wwidth :initform 123 :accessor poolevt-wwidth)
       (attack :initform 0 :accessor poolevt-attack)
       (release :initform 0.01 :accessor poolevt-release)
       (pan :initform 0.5 :accessor poolevt-pan)
       (snd-id :initform nil :accessor poolevt-snd-id)
       (adjust-stretch :initform nil :accessor poolevt-adjust-stretch)
       (out1 :initform 0 :accessor poolevt-out1)
       (out2 :initform 1 :accessor poolevt-out2))
    (:parameters time lsample keynum amp dy start end stretch wwidth attack release pan snd-id adjust-stretch out1 out2)
    (:event-streams))
  (defobject sampleevt (event)
      ((lsample :initform nil :accessor sampleevt-lsample)
       (keynum :initform nil :accessor sampleevt-keynum)
       (amp :initform 0.0 :accessor sampleevt-amp)
;;;       (transp :initform 0.0 :accessor sampleevt-transp)
       (duration :initform 1 :accessor sampleevt-duration)
       (start :initform 0 :accessor sampleevt-start)
       (out :initform 0 :accessor sampleevt-out))
    (:parameters time lsample keynum amp duration start out)
    (:event-streams)))

#|
(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))
|#

(defun function-name (fn)
  (cl-ppcre:regex-replace
   "^#<function \+\([^>]\+\)>"
   (format nil "~a" fn)
   "\\\1"))

;;; (function-name #'cl-sfz:play-sfz-one-shot)

(defun get-fn-from-string (str)
  "return function object from name given as string. For functions in
an external package, a leading <package-name>: has to be provided."
  (if (position #\: str)
      (destructuring-bind (name package)
          (read-from-string
           (cl-ppcre:regex-replace
            "^\([^:]\+\):\([^>]\+\)"
            str
            "(\"\\2\" \\1)"))
        (symbol-function (intern (string-upcase name) package)))
      (symbol-function
       (intern
        (string-upcase
         (cl-ppcre:regex-replace "^\([^>]\+\)" str "\\\1"))))))

(defun lsample->poolevt (lsample &key keynum (time 0) (startpos 0) dur (ampdb 0))
  (let* ((transp (if keynum (- keynum (of-incudine-dsps:lsample-keynum lsample))
                     0))
         (rate (ou:ct->fr transp))
         (bsr (incudine:buffer-sample-rate (of-incudine-dsps:lsample-buffer lsample)))
         (start (/ (* bsr startpos) incudine::*sample-rate*))
         (end (* (+ start (or dur
                              (/ (incudine:buffer-frames
                                  (of-incudine-dsps:lsample-buffer lsample))
                                 bsr)))
                 rate)))
    ;;    (break "~a" (eql play-fn #'sample-play))
    ;;            (format t "~a~%" (incudine::buffer-file buffer))
    (new poolevt
      :time time
      :lsample lsample
      :amp ampdb
      :keynum (or keynum (of-incudine-dsps:lsample-keynum lsample))
      :start start
      :end end
      :stretch (/ rate))))

(defun svg->poolevt (&rest args)
  "recreate a poolevt from the :attributes property of the svg element."
;;;  (if *debug* (format t "~&svg->poolevent: ~a~%" args))
;;;  (break "svg->poolevt: args: ~S" args)
  (ou:with-props (lsample lsample-keynum lsample-amp lsample-play-fn saved-keynum keynum
                       amp amplitude duration start end stretch loopstart loopend adjust-stretch)
      args
    (let ((buf (incudine-bufs:ensure-buffer lsample)))
      (let* ((buffer (if (consp buf) (first buf) buf))
             (new-lsample (of-incudine-dsps::make-lsample
                           :buffer buffer
                           :name (file-namestring (pathname lsample))
                           :keynum (float lsample-keynum 1.0d0)
                           :amp lsample-amp
                           :loopstart loopstart
                           :loopend loopend
                           :play-fn (svg-symbol->fn lsample-play-fn)))
             (bufdur (- (/ (incudine:buffer-frames buffer)
                           (incudine:buffer-sample-rate buffer))
                        start))
             (saved-dur (max 0.001 (* (- (if (zerop end) bufdur end) start) stretch)))
             (new-stretch (float (max 0.0001
                                      (* stretch (/ duration (max 0.001 saved-dur))
                                         (if adjust-stretch
                                             (expt 2 (/ (- keynum saved-keynum) -12))
                                             1)))
                                 1.0d0)))
        (apply #'make-instance 'poolevt
               (list* :lsample new-lsample
                      :stretch new-stretch
                      :keynum keynum
                      :amp (+ amp (opacity->db amplitude))
                      (ou:delete-props args :y2 :lsample :lsample-keynum :lsample-amp :lsample-play-fn
                                            :loopstart :loopend :amp
                                                     :saved-keynum :amplitude :duration :channel)))))))

#|
(opacity->db -9)
|#

(svg-ie:add-svg-attr-props-to-quote :lsample)

(add-svg-assoc-fns
 '((poolevt . svg->poolevt)))

(defmethod write-event ((obj poolevt) (fil svg-file) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the elements slot of the
svg-file."
  (with-slots (lsample amp keynum dy start end stretch wwidth attack release pan snd-id adjust-stretch out1 out2) obj
    (with-slots (of-incudine-dsps::buffer of-incudine-dsps::play-fn of-incudine-dsps::keynum of-incudine-dsps::loopstart of-incudine-dsps::loopend) lsample
      (let* ((myid (incudine-bufs:buffer-id incudine::buffer))
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
                                          of-incudine-dsps::keynum
                                          (of-incudine-dsps:lsample-amp lsample)
                                          (function-name of-incudine-dsps::play-fn)
                                          keynum
                                          amp
                                          start (if (= region-end bufdur) 0 region-end)
                                          stretch wwidth attack release pan out1 out2
                                          of-incudine-dsps::loopstart of-incudine-dsps::loopend
                                          dy
                                          id
                                          adjust-stretch)
                      :id (new-id fil 'line-ids)))))

        (svg-file-insert-line line (if (numberp myid) myid 2) fil)))))

(defun rt-write-poolevt (obj scoretime)
  "realtime-output of poolevt"
  (with-slots (lsample amp keynum start end
               stretch wwidth attack release pan out1 out2 snd-id)
      obj
    (let* ((buffer (of-incudine-dsps:lsample-buffer lsample))
           (time (+ (rts-now) (* *rt-scale* scoretime)))
           (transp (- keynum (of-incudine-dsps:lsample-keynum lsample)))
           (ampdb (+ amp (of-incudine-dsps:lsample-amp lsample)))
;;;           (out (mod snd-id 8))
           )
      ;; (if *debug* (format t "~&liner: ~S~%" (list :lsample-amp (of-incudine-dsps:lsample-amp lsample) :buffer buffer :amp amp
      ;;                                                  :transp transp :start start :end end
      ;;                                                  :stretch stretch :wwidth wwidth :attack attack
      ;;                                                  :release release :pan pan :out1 out1 :out2 out2)))
      (at time #'of-incudine-dsps::play-buffer-stretch-env-pan-out*
          :buffer buffer :env of-incudine-dsps:*env1* :amp ampdb
          :transp transp :start start :end end
          :stretch stretch :wwidth wwidth :attack attack
          :release release :pan pan :out1 out1 :out2 out2
          :head 200)

      ;; (at time #'cl-poolplayer::distributed-play (list :buffer buffer :amp amp
      ;;                                                  :transp transp :start start :end end
      ;;                                                  :stretch stretch :wwidth wwidth :attack attack
      ;;                                                  :release release :pan pan :out1 out :out2 (1+ out)))
      )))

(defmethod write-event ((obj poolevt) (to incudine-stream) scoretime)
  "realtime-output of poolevt"
  (declare (ignore to))
  (rt-write-poolevt obj scoretime))

(defmethod write-event ((obj poolevt) (to jackmidi:output-stream) scoretime)
  "realtime-output of poolevt"
  (declare (ignore to))
  (rt-write-poolevt obj scoretime))

