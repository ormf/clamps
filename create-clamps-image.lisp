;;; 
;;; create-clamps-image.lisp
;;;
;;; create image of clamps. Issue "sbcl --load
;;; create-cm-all-image.lisp" in this folder. This should generate a
;;; file "cm-all" which contains a lisp image with cm-all already
;;; loaded and the engines started.
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

(in-package :cl-user)

(mapc #'ql:quickload '(:slynk :clamps))

(setf sb-ext:*init-hooks* (append sb-ext:*init-hooks* (list #'of-incudine-dsps:restore-envs
                                                            #'ats-cuda-display:restore-tables
                                                            ;;; #'cl-user::clamps
                                                            #'cl-user::clamps-image-start
                                                            )))

(sb-ext:save-lisp-and-die
 (asdf:system-relative-pathname :clamps "clamps")
 :executable t
 :purify t
 :compression t)
