;;; 
;;; cl-poolplayer-extra.lisp
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

(in-package :cl-poolplayer)


(defun cm-collect-song (song)
  (let ((*events* '())
        (time (now)))
    (funcall (song-playfn song)
             (funcall (song-durfn song))
             :player-type 'eventplotter)
    (sort
     (mapcar #'(lambda (x)
                  ;;; recalc keynum from :transp and lsample-keynum then remove :transp from args
                 (let* ((lsample (getf (cdr x) :lsample))
                        (keynum (+ (incudine:lsample-keynum lsample) (getf (cdr x) :transp)))
                        (args (cdr x)))
                   (remf args :transp)
                   (setf (getf args :keynum) keynum)
                   (apply #'make-instance 'cm::poolevt
                          :time (float (- (first x) time) 1.0)
                          args)))
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
