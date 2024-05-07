;;; 
;;; utils.lisp
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

(in-package :clog-dsp-widgets)

(defun db-slider->amp (x &key (min -40) (max 12))
  (if (zerop x) 0 (ou:db->amp (ou:n-lin x min max))))

(defun amp->db-slider (amp &key (min -40) (max 12))
  (let ((range (- max min)))
    (ou:clip (/ (- (ou:amp->db amp) min) range) 0 1)))

(defun db-slider->db (slider-val &key (min -40) (max 12))
  (ou:n-lin slider-val min max))

(defun db->db-slider (db &key (min -40) (max 12))
  (let ((range (- max min)))
    (ou:clip (/ (- db min) range) 0 1)))

;;; (db->db-slider (db-slider->db 0.5))

