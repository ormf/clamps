;;; 
;;; clamps-utils.lisp
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

(in-package :clamps)

(defparameter %clamps-version% 1)

(defun clamps-version-number (&rest arg) arg %clamps-version%)

(defun clamps-version-name ()
  (format nil
          "~a.~a.~a"
          (ldb (byte 8 16) %clamps-version%)
          (ldb (byte 8 8) %clamps-version%)
          (ldb (byte 8 0) %clamps-version%)))

(defun clamps-version (&rest fmat)
  (cond ((null fmat)
         (format nil "Clamps ~a" (ou:system-version :clamps)))
        ((not (null (cdr fmat)))
         (error "clamps-version: more than one arg: ~s." fmat))
        ((eq (car fmat) ':number) %clamps-version%)
        ((eq (car fmat) ':string) (clamps-version-name))
        ((eq (car fmat) ':list)
         (list (ldb (byte 8 16) %clamps-version%)
               (ldb (byte 8 8) %clamps-version%)
               (ldb (byte 8 0) %clamps-version%)))
        (t (error "clamps-version: Bad format: ~s." (car fmat)))))

(defparameter *clamps-logo* t)

(defun clamps-logo ()
  "draw clamps logo on *standard-output* the nerdy way. Originally written
by Tobias Kunze. Some cleanup done by Orm Finnendahl."
  (if *clamps-logo*
      (let ((e "~%"))
        (format t e)
        (do ((v (make-string 15)) (y 0 (+ y 1)))
            ((= y 7) nil)
          (format t
                  (do ((x 0 (+ x 1)))
                      ((= x 15) (cond
                                  ((= y 2)
                                   (concatenate
                                    'string v " CLAMPS" e))
                                  ((= y 3)
                                   (concatenate
                                    'string v " Common Lisp Aided Music Production System" e))
                                  ((= y 4)
                                   (concatenate
                                    'string v " Version " (clamps-version-name) e))
                                  (t (concatenate 'string v e))))
                    (setf (elt v x)
                          (cond
                            ((<= 2 (- x y) 4) #\\) 
                            ((= (- x (- 4 (mod (+ 13 y) 15))) 1) #\/)
                            ((<= 1 y 5) #\-)
                            ((= (* (- x 6) (- y 3)) 15) #\/)
                            (:else #\ ))))))
        (format t e)))
  (values))

(defun clamps:idump (node)
  (unless incudine.util:*logger-stream*
    (reset-logger-stream))
  (dump (incudine:node node)))

(defun clamps:set-tempo (bpm)
  (setf cm:*tempo* bpm)
  (setf (bpm *tempo*) bpm))

(defvar *clamps-doc-acceptor* (make-instance 'hunchentoot:easy-acceptor
        :port 8282
        :document-root (asdf:system-relative-pathname :clamps "doc/")))

(defun start-doc-acceptor ()
  (unless (hunchentoot::acceptor-listen-socket *clamps-doc-acceptor*)
    (hunchentoot:start *clamps-doc-acceptor*)))

(setf (fdefinition 'clamps::set-bpm) #'clamps:set-tempo)

(defun n-lin-bp (x bp min max)
  (n-lin (apply #'interp x (flatten bp)) min max))

(defun n-exp-bp (x bp min max)
  (n-exp (apply #'interp x (flatten bp)) min max))

(defun plot-2d (seq)
  "plot a linear sequence by grouping the elements in 2."
  (plot (ou:group seq 2))
  (values))

(defun plot-3d (seq)
  "plot a linear sequence by grouping the elements in 3."
  (plot (ou:group seq 3))
  (values))

(defvar *standard-pitch* 440.0)

(defun set-standard-pitch (freq)
  (setf *standard-pitch* (float freq 1.0))
  (setf oid::*standard-pitch* (float freq 1.0)))

(defun ftom (f &key (tuning-base *standard-pitch*))
  (+ 69 (* 12 (log (/ f tuning-base) 2))))

(defun mtof (m &key (tuning-base *standard-pitch*))
  (* tuning-base (expt 2 (/ (- m 69) 12))))

(defun fr2ct (fr)
  "Return interval in midcent of frequency ratio fr."
  (* 12 (log fr 2)))

(defun ct2fr (ct)
  "Return frequency ratio of interval ct in midcent."
  (expt 2 (/ ct 12)))
