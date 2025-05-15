;;; 
;;; midi-port.lisp
;;;
;;; Infrastructure for midi-in and midi-out.
;;;
;;; **********************************************************************
;;; Copyright (c) 2025 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-midictl)

(defparameter *midi-ports* nil)

(defun find-midi-port (id)
  (getf *midi-ports* id))

(defun open-midi-port (id)
  "open a new midi port by creating its struct. Return the struct."
  (if (find-midi-port id) (error "midi-port ~S already open" id)
      (setf *midi-ports* (list* id
                                (let ((in-name (format nil "~a-in" id))
                                      (out-name (format nil "~a-out" id)))
                                  (make-midi-port :id id
                                                  :in (jackmidi:open :port-name in-name)
                                                  :out (jackmidi:open :direction :output :port-name out-name)))
                                *midi-ports*))))

(defun close-midi-port (id)
  "open a new midi port by creating its struct. Return the struct."
  (let ((midi-port (find-midi-port id)))
    (unless midi-port  (error "midi-port ~S not found" id))
    (jackmidi:close (midi-port-in midi-port))
    (jackmidi:close (midi-port-out midi-port))
    (remf *midi-ports* id) ^))

;;; *midi-ports*

;;; '(#xF0 #x00 #x00 #x66 #x10 #x12 #x00 #x48 #x65 #x6C #x6C #x6F #xF7)

 ; => (240 0 0 102 16 18 0 72 101 108 108 111 247)

(open-midi-port :midi-1)
(open-midi-port :midi-2)
(open-midi-port :midi-3)

(find-midi-port :midi-1)

(incudine:midiout-sysex)

(progn
  (close-midi-port :midi-1)
  (close-midi-port :midi-2)
  (close-midi-port :midi-3))

(char-code #\SPACE) ;; 32

(char-code #\@) 64
(char-code #\A)

(defparameter *mackie-sysex-header* '(#xF0 #x00 #x00 #x66 #x10))
(midiout-sysex (append *mackie-sysex-header* '(#x12 0) (subseq (map 'list #'char-code "Dies ist ein Test, was man alles mit dem Asparion anstellen kann und ich bin bereits einigermaßen zufrieden :-)") 0 (* 7 3)) '(#xF7)) (midi-port-out (find-midi-port :midi-1)))


(midiout-sysex (append *mackie-sysex-header* '(#x12 21) (subseq (map 'list #'char-code " was man alles mit dem Asparion anstellen kann und ich bin bereits einigermaßen zufrieden :-)") 0 (* 7 3)) '(#xF7)) (midi-port-out (find-midi-port :midi-1)))

(loop for x below 16
      do (midiout-sysex (append *mackie-sysex-header* `(#x12 ,(* 7 (mod x 8))) (map 'list #'char-code (format nil "~7a" (format nil "Kan ~2,'0d " (1+ x)))) '(#xF7))
                        (midi-port-out (find-midi-port (if (< x 8) :midi-1 :midi-2)))))

(loop for instr in '("Fl" "Kl" "Ob" "Fg" "Tr" "Hrn" "Pos" "Tb" "Szg1" "Szg2" "Pno" "Vn1" "Vn2" "Va" "Vc" "Kb")
      for x below 16
      do (midiout-sysex (append *mackie-sysex-header* `(#x12 ,(+ 56 (* 7 (mod x 8)))) (map 'list #'char-code (format nil "~7a" instr)) '(#xF7))
                        (midi-port-out (find-midi-port (if (< x 8) :midi-1 :midi-2)))))

(defun asparion-write-upper-line (text idx midi-1 midi-2)
  (midiout-sysex (append *mackie-sysex-header* `(#x12 ,(* 7 (mod idx 8))) (map 'list #'char-code (format nil "~7a" text)) '(#xF7))
                 (if (< idx 8) midi-1 midi-2)))

(defun asparion-write-lower-line (text idx midi-1 midi-2)
  (midiout-sysex (append *mackie-sysex-header* `(#x12 ,(+ 56 (* 7 (mod idx 8)))) (map 'list #'char-code (format nil "~7a" text)) '(#xF7))
                        (if (< idx 8) midi-1 midi-2)))

(midi-port-out (find-midi-port (if (< x 8) :midi-1 :midi-2)))

(let ((midi-1 (midi-port-out (find-midi-port :midi-1)))
      (midi-2 (midi-port-out (find-midi-port :midi-2))))
  (dotimes (x 16) (asparion-write-upper-line (format nil "~2,'0d" (1+ x)) x midi-1 midi-2))
  (loop
    for instr in '("Fl" "Kl" "Ob" "Fg" "Tr" "Hrn" "Pos" "Tb" "Szg1" "Szg2" "Pno" "Vn1" "Vn2" "Va" "Vc" "Kb")
    for x below 16
    do (asparion-write-lower-line instr x midi-1 midi-2)))

(defun pad-string)

(midiout-sysex (append *mackie-sysex-header* `(#x20 0 6 #xF7)) (midi-port-out (find-midi-port :midi-1)))

(midiout-sysex (append *mackie-sysex-header* `(#x21 1 #xF7)) (midi-port-out (find-midi-port :midi-1)))
(midiout-sysex `(#xD0 #x0c) (midi-port-out (find-midi-port :midi-1)))
(progn
  (cm::at (cm::now) #'midiout-sysex `(#xD0 #x0c) (midi-port-out (find-midi-port :midi-1)))
  (cm::at (+ (cm::now) 0.1) #'midiout-sysex `(#xD0 #x0c) (midi-port-out (find-midi-port :midi-1))))

(format nil "~7a" "hallo")

#x37
(append *mackie-sysex-header* '(#x12 0) (map 'list #'char-code "hallo"))
