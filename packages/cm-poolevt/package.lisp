;;; 
;;; package.lisp
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

(in-package :cm)

(shadow '(play-sfz-one-shot play-sfz-loop) 'cm)
(use-package '(incudine-bufs of-incudine-dsps))
(export '(svg->poolevt poolevt poolevt-lsample poolevt-keynum poolevt-buffer-idx
          poolevt-amp poolevt-start poolevt-end poolevt-stretch poolevt-wwidth
          poolevt-attack poolevt-release poolevt-pan poolevt-out1 poolevt-out2 lsample->poolevt))

;;; (use-package :cl-sfz)

