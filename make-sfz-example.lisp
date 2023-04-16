;;; 
;;; make-sfz-example.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2022 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-sfz)

#|

- einen Ordner mit Instrumentennamen vorbereiten, in dem sich
  sämtliche Samples im Unterordner "samples" befinden.

- Die Samples sollten mit einer 3-stelligen Miditonhöhe beginnen und
  von jeder Tonhöhe sollte nur maximal ein Sample vorhanden sein.

Anschließend werden sfz Dateien folgendermaßen generiert:

(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-pp" "bassoboe-pp")

(write-sfz "~/work/snd/sfz/oud" "oud" "oud")
(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-f" "bassoboe-f")

|#
