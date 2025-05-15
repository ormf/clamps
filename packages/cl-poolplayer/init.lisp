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

(setf *players* (make-array 16
                            :element-type 'eventplayer
                            :initial-contents
                            (loop
                              for x below 16
                              collect (make-instance 'eventplayer))))
