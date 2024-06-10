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

(defun system-version (system-designator)
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))


(defun clamps-version (&rest fmat)
  (cond ((null fmat)
         (format nil "Clamps ~a" (system-version :clamps)))
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
