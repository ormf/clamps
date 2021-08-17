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

(defobject poolevt (event)
    ((lsample :initform nil :accessor poolevt-lsample)
     (keynum :initform nil :accessor poolevt-keynum)
     (amp :initform 0.0 :accessor poolevt-amp)
     (transp :initform 0.0 :accessor poolevt-transp)
     (start :initform 0 :accessor poolevt-start)
     (end :initform 0 :accessor poolevt-end)
     (stretch :initform 1.0 :accessor poolevt-stretch)
     (wwidth :initform 123 :accessor poolevt-wwidth)
     (attack :initform 0 :accessor poolevt-attack)
     (release :initform 0.01 :accessor poolevt-release)
     (pan :initform 0.5 :accessor poolevt-pan)
     (out1 :initform 0 :accessor poolevt-out1)
     (out2 :initform 1 :accessor poolevt-out2))
  (:parameters time lsample keynum amp transp start end stretch wwidth attack release pan out1 out2)
  (:event-streams))

(eval-when (:compile-toplevel :load-toplevel)
  (defobject poolevt (event)
      ((lsample :initform nil :accessor poolevt-lsample)
       (keynum :initform nil :accessor poolevt-keynum)
       (amp :initform 0.0 :accessor poolevt-amp)
       (transp :initform 0.0 :accessor poolevt-transp)
       (start :initform 0 :accessor poolevt-start)
       (end :initform 0 :accessor poolevt-end)
       (stretch :initform 1.0 :accessor poolevt-stretch)
       (wwidth :initform 123 :accessor poolevt-wwidth)
       (attack :initform 0 :accessor poolevt-attack)
       (release :initform 0.01 :accessor poolevt-release)
       (pan :initform 0.5 :accessor poolevt-pan)
       (out1 :initform 0 :accessor poolevt-out1)
       (out2 :initform 1 :accessor poolevt-out2))
    (:parameters time lsample keynum amp transp start end stretch wwidth attack release pan out1 out2)
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

#|
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
|#

(defun svg->poolevt (&rest args)
  "recreate a poolevt from the :attributes property and the coordinates of
the svg element."
  (if *debug* (format t "~&svg->poolevent: ~a~%" args))
  (ou:with-props (lsample play-fn keynum amplitude sample-amp stretch pitch) args
    (let ((new-lsample (let ((file (format nil "~a" lsample)))
                         (apply #'incudine::make-lsample
                                (list* :buffer (incudine:find-buffer file)
                                       :filename file
                                       :amp sample-amp
                                       :play-fn (svg-symbol->fn play-fn)
                                       :keynum (float pitch 1.0d0)
                                       (ou:get-props-list args '(:loopstart :loopend))))))
          (transp (- keynum pitch)))
      (apply #'make-instance 'poolevt
             (list* :lsample new-lsample :transp transp
                    :stretch (* stretch (expt 2 (/ transp -12)))
                    :amp (ou:amp->db (* sample-amp amplitude))
                    (ou:get-props-list args '(:time :keynum :start :end :wwidth :attack :release :pan :out1 :out2)))))))

;;; (mapc #'remove-svg-assoc-fn '(poolevt play-sfz-one-shot play-sfz-loop))
;;; (mapcar #'remove-svg-assoc-fn '(midi))

(add-svg-assoc-fns
 `((poolevt . ,(symbol-function 'svg->poolevt))
   (play-sfz-one-shot . ,(symbol-function 'cl-sfz:play-sfz-one-shot))
   (play-sfz-loop . ,(symbol-function 'cl-sfz:play-sfz-loop))))

(svg-ie:add-svg-attr-props-to-quote :lsample)

(defmethod write-event ((obj poolevt) (fil svg-file) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (with-slots (lsample amp transp start end stretch wwidth attack release pan out1 out2) obj
    (with-slots (incudine::filename incudine::buffer incudine::play-fn incudine::keynum incudine::loopstart incudine::loopend) lsample
      (let* ((myid (incudine:buffer-id incudine::buffer))
             (x-scale (x-scale fil))
             (stroke-width 0.5)
             (color (chan->color (if (numberp myid) myid 2)))
             (line (let ((x1 (* x-scale scoretime))
                         (y1 (+ incudine::keynum transp))
                         (width
                           (* x-scale
                              stretch
                              (- (if (zerop end)
                                     (float (/ (incudine::buffer-size incudine::buffer)
                                               (incudine:buffer-sample-rate incudine::buffer))
                                            1.0)
                                     end)
                                 start)))
                         (opacity (ou:db->amp amp) 1.0))
                     (make-instance
                      'svg-ie::svg-line
                      :x1 (float x1 1.0) :y1 (float y1 1.0)
                      :x2 (float (+ x1 width) 1.0) :y2 (float y1 1.0)
                      :stroke-width stroke-width
                      :opacity opacity
                      :stroke-color color 
                      ;; :fill-color color
                      :attributes (format nil ":type poolevt :lsample ~a :keynum ~a :amp ~a :start ~a :end ~a :stretch ~a :wwidth ~a :attack ~a :release ~a :pan ~a :out1 ~a :out2 ~a :play-fn ~a :loopstart ~a :loopend ~a :sample-amp ~a
"
                                          incudine::filename
                                          incudine::keynum
                                          amp
                                          start end (* stretch (expt 2 (/ transp 12))) wwidth attack release pan out1 out2
                                          (function-name incudine::play-fn)
                                          incudine::loopstart incudine::loopend
                                          (incudine:lsample-amp lsample))
                      :id (new-id fil 'line-ids)))))
;;;      (break "line: ~a, obj: ~a ~a ~a" line buffer-file cl-poolplayer:*pool-hash* (gethash buffer-file cl-poolplayer:*pool-hash*))
        (if *debug* (format t "~&obj: ~a~%" obj))
        (svg-file-insert-line line (if (numberp myid) myid 2) fil)))))

(defmethod write-event ((obj poolevt) (to incudine-stream) scoretime)
  "convert a poolevt object into a freshly allocated svg-line object and
insert it at the appropriate position into the events slot of
svg-file."
  (with-slots (lsample amp transp start end
               stretch wwidth attack release pan out1 out2)
      obj
    (let* ((buffer (incudine:lsample-buffer lsample))
           (time (+ (rts-now) (* *rt-scale* scoretime)))
           (amplitude (+ (ou:amp->db (incudine:lsample-amp lsample)) amp)))
      (if *debug* (format t "~&line: ~S~%" (list :buffer buffer :amp amp
                                                       :transp transp :start start :end end
                                                       :stretch stretch :wwidth wwidth :attack attack
                                                       :release release :pan pan :out1 out1 :out2 out2)))
      (at time #'cl-poolplayer::distributed-play (list :buffer buffer :amp amplitude
                                                       :transp transp :start start :end end
                                                       :stretch stretch :wwidth wwidth :attack attack
                                                       :release release :pan pan :out1 out1 :out2 out2)))))
;;; (aref cl-poolplayer::*buffer-idxs*

(export '(poolevt poolevt-buffer-idx poolevt-amp poolevt-start poolevt-end poolevt-stretch poolevt-wwidth poolevt-attack poolevt-release poolevt-pan poolevt-out1 poolevt-out2) 'cm)

(in-package :cl-poolplayer)

(defun cm-collect-song (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort
     (mapcar #'(lambda (x) (apply #'make-instance 'cm::poolevt
                             :time (float (- (first x) time) 1.0)
                             (cdr x)
                             ;; :lsample (getf (cdr x) :lsample)
                             ;; (progn (remf (cdr x) :buffer)
                             ;;        (cdr x))
                             ))
             *events*)
     #'< :key (lambda (x) (sv x cm::time)))))

(defmacro collecting-cm (&rest body)
  `(let ((*events* '())
         (time (now)))
     ,@body
     (sort
      (mapcar #'(lambda (x) (apply #'make-instance 'cm::poolevt
                             :time (float (- (first x) time) 1.0)
                             (cdr x)))
              *events*)
      #'< :key (lambda (x) (sv x cm::time)))))



(export '(cm-collect-song collecting-cm) 'cl-poolplayer)
