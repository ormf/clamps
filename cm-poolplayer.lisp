;;; 
;;; cm-poolplayer.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defparameter *debug* nil)

#|
(defobject poolevt (event)
      ((lsample :initform nil :accessor poolevt-lsample)
       (keynum :initform nil :accessor poolevt-keynum)
       (amp :initform 0.0 :accessor poolevt-amp)
;;;       (transp :initform 0.0 :accessor poolevt-transp)
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
|#

(eval-when (:compile-toplevel :load-toplevel)
  (defobject poolevt (event)
      ((lsample :initform nil :accessor poolevt-lsample)
       (keynum :initform nil :accessor poolevt-keynum)
       (amp :initform 0.0 :accessor poolevt-amp)
;;;       (transp :initform 0.0 :accessor poolevt-transp)
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
    (:event-streams)))

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))

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

(defun cm::lsample->poolevt (lsample pitch &key time (startpos 0) (dur 1) (ampdb 0))
  (let* ((transp (- pitch (incudine:lsample-keynum lsample)))
         (rate (ou:ct->fv transp))
         (bsr (incudine:buffer-sample-rate (incudine:lsample-buffer lsample)))
         (start (/ (* bsr startpos) incudine::*sample-rate*))
         (end (* (+ start dur) rate)))
    ;;    (break "~a" (eql play-fn #'sample-play))
    ;;            (format t "~a~%" (incudine::buffer-file buffer))
    (new poolevt
      :time time
      :lsample lsample
      :amp ampdb
      :keynum pitch
      :start start
      :end end
      :stretch (/ rate))))

(defun svg->poolevt (&rest args)
  "recreate a poolevt from the :attributes property of the svg element."
  (if *debug* (format t "~&svg->poolevent: ~a~%" args))
;;;  (break "svg->poolevt: args: ~S" args)
  (ou:with-props (lsample lsample-keynum lsample-amp lsample-play-fn saved-keynum keynum
                          duration start end stretch loopstart loopend adjust-stretch)
      args
    (let* ((file (format nil "~a" lsample))
           (buffer (incudine:find-buffer file))
           (new-lsample (incudine::make-lsample
                         :buffer buffer
                         :filename file
                         :keynum (float lsample-keynum 1.0d0)
                         :amp lsample-amp
                         :loopstart loopstart
                         :loopend loopend
                         :play-fn (svg-symbol->fn lsample-play-fn)))
           (bufdur (- (/ (incudine:buffer-frames buffer)
                         (incudine:buffer-sample-rate buffer))
                      start))
           (saved-dur (* (- (if (zerop end) bufdur end) start) stretch))
           (new-stretch (* stretch (/ duration saved-dur)
                           (if adjust-stretch
                               (expt 2 (/ (- keynum saved-keynum) -12))
                               1))))
      (apply #'make-instance 'poolevt
             (list* :lsample new-lsample
                    :stretch new-stretch
                    (ou:delete-props args :lsample :lsample-keynum :lsample-amp :lsample-play-fn
                                          :loopstart :loopend
                                          :saved-keynum :amplitude :duration :channel))))))

#|

|#

(add-svg-assoc-fns
 `((poolevt . ,#'svg->poolevt)))

#|

(defun svg->cm (file layer x-scale &key colormap start end)
  (let* ((x-offs (if start (* -1 (/ start x-scale)) 0))
         (ende (if end (+ x-offs (/ end x-scale)) most-positive-fixnum)))
;;;    (break "x-offs: ~a ende: ~a" x-offs ende)
    (mapcar
     (lambda (line) (ou:with-props (x1 y1 x2 color opacity attributes) line
                 (if (getf attributes :lsample)
                     (new poolevt
                       :time
                       :buffer-file (getf attributes :lsample)
                       :amp
                       :transp
                       :start
                       :end
                       :buffer-length
                       :stretch
                       :wwidth
                       :attack
                       :release
                       :pan
                       :out1
                       :out2
                       
                       )
                        (new midi
                          :time (float (* x-scale x1))
                          :keynum y1
                          :duration (float (* x-scale (- x2 x1)))
                          :amplitude opacity
                          :channel (color->chan color colormap)))))
     (remove-if-not (lambda (line) (<= 0 (getf line :x1) ende))
                    (svg-ie::svg->lines :infile file :layer layer :xquantize nil :yquantize nil :x-offset x-offs)))))
|#

(defmethod write-event ((obj poolevt) (fil svg-file) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the elements slot of the
svg-file."
  (with-slots (lsample amp keynum dy start end stretch wwidth attack release pan snd-id adjust-stretch out1 out2) obj
    (with-slots (incudine::filename incudine::buffer incudine::play-fn incudine::keynum incudine::loopstart incudine::loopend) lsample
      (let* ((myid (incudine:buffer-id incudine::buffer))
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
                         (opacity (ou:db->amp amp) 1.0))
                     (make-instance
                      'svg-ie::svg-line
                      :x1 (float x1 1.0) :y1 (float y1 1.0)
                      :x2 (float (+ x1 width) 1.0) :y2 (float (+ y1 dy) 1.0)
                      :stroke-width stroke-width
                      :opacity opacity
                      :stroke-color color 
                      ;; :fill-color color
                      :attributes (format nil ":type poolevt :lsample ~a :lsample-keynum ~a :lsample-amp ~a :lsample-play-fn ~a :keynum ~a :amp ~a :start ~a :end ~a :stretch ~a :wwidth ~a :attack ~a :release ~a :pan ~a :out1 ~a :out2 ~a :loopstart ~a :loopend ~a :dy ~a :snd-id ~a :adjust-stretch ~a"
                                          incudine::filename
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
;;;      (break "line: ~a ~a ~a ~a" line incudine::buffer dur stretch)
        (if *debug* (format t "~&obj: ~a~%" obj))
        (svg-file-insert-line line (if (numberp myid) myid 2) fil)))))

(defmethod write-event ((obj poolevt) (to incudine-stream) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (with-slots (lsample amp keynum start end
               stretch wwidth attack release pan out1 out2)
      obj
    (let* ((buffer (incudine:lsample-buffer lsample))
           (time (+ (rts-now) (* *rt-scale* scoretime)))
           (transp (- keynum (incudine:lsample-keynum lsample))))
      (if *debug* (format t "~&line: ~S~%" (list :buffer buffer :amp amp
                                                       :transp transp :start start :end end
                                                       :stretch stretch :wwidth wwidth :attack attack
                                                       :release release :pan pan :out1 out1 :out2 out2)))
      (at time #'cl-poolplayer::distributed-play (list :buffer buffer :amp amp
                                                       :transp transp :start start :end end
                                                       :stretch stretch :wwidth wwidth :attack attack
                                                       :release release :pan pan :out1 out1 :out2 out2)))))
;;; (aref cl-poolplayer::*buffer-idxs*

(export '(poolevt poolevt-buffer-idx poolevt-amp poolevt-start poolevt-end poolevt-stretch poolevt-wwidth poolevt-attack poolevt-release poolevt-pan poolevt-out1 poolevt-out2 lsample->poolevt) 'cm)
