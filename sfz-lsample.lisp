;;; 
;;; sfz-lsample.lisp
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

(in-package :incudine)

(defun abs-path (sample-path sfz-file-path)
  (merge-pathnames sample-path sfz-file-path))

(defun get-keynum (entry)
  (sample (- (or (getf entry :pitch-keycenter) 60) (/ (or (getf entry :tune) 0) 100))))

(defun sfz->lsample (sfz-entry dir &key (play-fn #'cl-sfz:play-sfz-loop))
  (let* ((abs-filepath (abs-path (getf sfz-entry :sample) dir))
         (buffer (of-buffer-load abs-filepath)))
    (make-lsample
     :filename abs-filepath
     :buffer buffer
     :play-fn play-fn
     :keynum (get-keynum sfz-entry)
     :amp (incudine::sample (getf sfz-entry :volume 0))
     :loopstart (sample (or (getf sfz-entry :loop-start) 0))
     :loopend (sample (or (getf sfz-entry :loop-end) (buffer-frames buffer))))))

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))
