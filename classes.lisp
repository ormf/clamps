;;; 
;;; classes.lisp
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

(defclass eventplayer ()
  ((playing :accessor :playing :initform t :initarg :playing)
   (dur :accessor :dur :initform 3 :initarg :dur)
   (end :accessor :end :initform 0 :initarg :end)
   (time :accessor :time :initform 0 :initarg :time)
   (preset :accessor :preset :initarg :preset)))

(defclass eventplotter ()
  ((playing :accessor :playing :initform t :initarg :playing)
   (dur :accessor :dur :initform 3 :initarg :dur)
   (end :accessor :end :initform 0 :initarg :end)
   (time :accessor :time :initform 0 :initarg :time)
   (preset :accessor :preset :initarg :preset)))

(defgeneric stop (p))

(defmethod stop ((p eventplayer))
  (sv p :playing nil))

(defgeneric preset-play (player preset dur &rest args))

(defmethod preset-play ((p eventplayer) preset dur &rest args)
  (sv p
      :playing t
    :end (if dur (+ (now) dur))
    :dur dur
    :preset preset)
  (funcall #'perform p (now) args))

(defmethod preset-play ((p eventplotter) preset dur &rest args)
  (sv p
      :playing t
    :end (if dur (+ (getf args :time) dur))
    :dur dur
    :preset preset)
  (funcall #'perform p (getf args :time) args))
