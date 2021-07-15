;;; 
;;; init.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-poolplayer)

(setf *print-case* :downcase)

(defparameter *sound-type-dirs* nil)

(rts :rt-wait 5)

;;; (setf *buffers* (load-sounds "../../snd/*.wav"))

(setf *players* (make-array 16
                            :element-type 'eventplayer
                            :initial-contents
                            (loop
                              for x below 16
                              collect (make-instance 'eventplayer))))

(defun start-bo-seqs (time) 
  (start-seq #'main-seq-01)
  (at (+ time (+ 30 (random 30))) #'start-seq #'main-seq-02)
  (at (+ time (+ 10 (random 20))) #'start-seq #'main-seq-03))

;;; (start-seq #'main-seq-02)

;;; (cd "/home/orm/work/programmieren/lisp/cl-poolplayer")
(defun init-poolplayer ()
  (load-presets "presets/cl-poolplayer-01.lisp")
;;;  (init-netsend)
  (init-midi)
  (def-sequences)
  (stop-all-seqs)
  ;;  (audio-test 13)
  )
;;; (incudine:rt-start)
;;; (init-poolplayer)
;; (start-bo-seqs (+ (now) 10))

#|

(show-playing-seqs)
(setf *emcs-conn* swank::*emacs-connection*)

(defun delete-other-windows
    (let ((swank::*emacs-connection* *emcs-conn*))
      (swank::eval-in-emacs '(delete-other-windows) t)))
|#
;;; (at (+ (now) 10) #'delete-other-windows))


#|
(show-playing-seqs)
|#
;;; (at (+ (now  0)) #'start-bo-seqs (+ (now) 0))

;;; (at (+ (now) 5) #'init-bo (+ (now) 5))

;;; (funcall (song-playfn e-guitar-01) 20)

;;; (show-playing-seqs)
;;; (setf *show-song* t)
;;; (setf *show-song* nil)

;;; (stop-all-seqs)
#|
(make-seq-player #'main-seq-01)

*remote-ip*

|#
