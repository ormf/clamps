;;; 
;;; example.lisp
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


;;; first initialize all synths which copy the input 0-7 to bus 0-7
;;; and output 0-7 to bus 7-15

(progn
  (node-free-all)
  (setup-io))

;;; check here, if the synths and groups are created:

(dump (node 0))

#|

Evaluating the expression above should result in the following
output in the repl:

group 0
    group 100
        node 1
          clear-buses 0 32
        node 2
          cp-input-buses 0
    group 200
    group 300
        node 3
          mix-bus-to-out 16 8
        node 4
          cp-output-buses 8
    group 400

|#


;;; define the storage places for the levelmeter values.

(defparameter *in-refs*
  (make-array 8 :initial-contents (loop repeat 8
                                        collect (make-ref -100.0d0))))

(defparameter *out-refs*
  (make-array 8 :initial-contents (loop repeat 8
                                        collect (make-ref -100.0d0))))

;;; define a gui with two levelmeters, one for the input and one for
;;; the output:

(defun new-window (body)
  "On-new-window handler."
  (setf (title (html-document body)) "Levelmeter Test")
  (levelmeter-gui :lm-in body :group 100 :refs *in-refs* :num 8)
  (levelmeter-gui :lm-out body :audio-bus 8 :group 300 :refs *out-refs* :num 8))

;;; open the gui in a browser
(start)


#|
;;; (setf *debug* nil)

(remove-dsp :lm-in)
(remove-dsp :lm-out)

(find-dsp :lm-in)
(find-dsp :lm-out)

(dump (node 0))
|#
