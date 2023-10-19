;;; 
;;; example.lisp
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


(unless (find-package :cl-midictl) (ql:quickload "cl-midictl"))
(in-package :cl-midictl)

(rt-start)

(setf *midi-in1* (jackmidi:open :direction :input
                                :port-name "midi_in_1"))

(setf *midi-out1* (jackmidi:open :direction :output
                                 :port-name "midi_out_1"))


;;; connect midi device with jackmidi port "midi_in_1"

(start-midi-receive *midi-in1*)

(setf *midi-debug* t)

;;; adding a generic controller for this input:

(add-midi-controller 'midi-controller :midi-in *midi-in1* :id :nk2
                     :cc-map #(0 1 2 3 4 5 6 7 16 17 18 19 20 21 22 23 8 9 10 11 12 13 14 15 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127))

(remove-midi-controller :nk2)

(setf (aref (cc-fns (find-controller :nk2)) 0) (lambda (cc-val) (format t "val: ~a~%" cc-val)))



(remhash :nk2 *midi-controllers*)
