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

(export '(
;;; svg->poolevt poolevt poolevt-lsample poolevt-keynum poolevt-buffer-idx poolevt-amp poolevt-start poolevt-end poolevt-stretch poolevt-wwidth poolevt-attack poolevt-release poolevt-pan poolevt-out1 poolevt-out2 lsample->poolevt 
          export-poolplayer-events)
        'cm)

(defun export-poolplayer-events (&key (evts cl-poolplayer:*poolplayer-events*) (file "/tmp/test.svg"))
  (let* ((seq (reverse evts))
         (start (caar seq)))
    (cm:events
     (mapcar (lambda (evt)
               (let* ((new (copy-list evt))
                      (time (- (first new) start))
                      (keynum (+ (getf (cdr new) :transp)
                                 (of-incudine-dsps:lsample-keynum
                                  (getf (cdr new) :lsample)))))
                 (remf (cdr new) :buffer)
                 (remf (cdr new) :transp)
;;;                 (setf (getf (cdr new) :amp) (ou:db->amp (getf (cdr new) :amp)))
                 (apply #'make-instance 'poolevt :time time :keynum keynum (cdr new))))
             seq)
     (namestring file))))
