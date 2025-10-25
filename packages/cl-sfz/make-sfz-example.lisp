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

;;; Beispiel für sol Dateien:

(loop for (dir template) in '(("~/work/snd/sfz/violin/ponticello/" "Vn-pont")
                              ("~/work/snd/sfz/viola/ponticello/" "Va-pont")
                              ("~/work/snd/sfz/violoncello/ponticello/" "Vc-pont")
                              ("~/work/snd/sfz/doublebass/ponticello/" "Cb-pont"))
      do (sol->sfz dir (string-downcase template) template))

(loop for (dir template) in '(("~/work/snd/sfz/violin/tasto/" "Vn-tasto")
                              ("~/work/snd/sfz/viola/tasto/" "Va-tasto")
                              ("~/work/snd/sfz/violoncello/tasto/" "Vc-tasto")
                              ("~/work/snd/sfz/doublebass/tasto/" "Cb-tasto"))
      do (sol->sfz dir (string-downcase template) template))

(loop for (dir template) in '(("~/work/snd/sfz/violin/ponticello/" "Vn-pont")
                              ("~/work/snd/sfz/viola/ponticello/" "Va-pont")
                              ("~/work/snd/sfz/violoncello/ponticello/" "Vc-pont")
                              ("~/work/snd/sfz/doublebass/ponticello/" "Cb-pont"))
      do (dolist (string '("1c" "2c" "3c" "4c"))
           (write-sfz dir (format nil "~(~a-~a~)" template string) string)))

(loop for (dir template) in '(("~/work/snd/sfz/violin/tasto/" "Vn-tasto")
                              ("~/work/snd/sfz/viola/tasto/" "Va-tasto")
                              ("~/work/snd/sfz/violoncello/tasto/" "Vc-tasto")
                              ("~/work/snd/sfz/doublebass/tasto/" "Cb-tasto"))
      do (dolist (string '("1c" "2c" "3c" "4c"))
           (write-sfz dir (format nil "~(~a-~a~)" template string) string)))


(loop for (dir template) in '(("~/work/snd/sfz/violin/tasto/" "Vn-tasto"))
      do (sol->sfz dir (string-downcase template) template))

(loop for (dir template) in '(("~/work/snd/sfz/violin/tasto/" "Vn-tasto"))
      do (dolist (string '("1c" "2c" "3c" "4c"))
           (write-sfz dir (format nil "~(~a-~a~)" template string) string)))

(loop for (dir template) in '(("~/work/snd/sfz/violin/artificial-harmonic/" "Vn-art-harm"))
      do (progn
           (sol->sfz dir (string-downcase template) template)
           (dolist (string '("1c" "2c" "3c" "4c"))
             (write-sfz dir (format nil "~(~a-~a~)" template string) string))))

(write-sfz "~/work/snd/sfz/bd/" "bd" "bd")

(cdr (assoc #\C '((#\C . 0) (#\D . 2))))

(defun parse-pitch (str)
  "Return MIDI keynum of /str/ of the form \"C4\" \"C#5\" \"Ab3\". \"C4\"
equals 60."
  (let ((alteration
          (if (= 3 (length str))
              (if (eql (aref str 1) #\#) 1 -1)
              0)))
    (+ 12
       alteration
       (getf
        '(#\C 0 #\D 2 #\E 4 #\F 5 #\G 7 #\A 9 #\B 11)
        (aref str 0))
       (* 12 (read-from-string (subseq str (1- (length str))))))))


#|

- einen Ordner mit Instrumentennamen vorbereiten, in dem sich
  sämtliche Samples im Unterordner "samples" befinden.

- Die Samples sollten mit einer 3-stelligen Miditonhöhe beginnen und
  von jeder Tonhöhe sollte nur maximal ein Sample vorhanden sein.

Anschließend werden sfz Dateien folgendermaßen generiert:

(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-pp" "bassoboe-pp")

(write-sfz "~/work/snd/sfz/oud" "oud" "oud")
(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-f" "bassoboe-f")



(dolist (dir (uiop/driver:subdirectories (pathname "~/work/snd/sfz/EiersheimerOrgel/")))
  (let ((dirname  (first (last (pathname-directory dir)))))
    (write-sfz dir dirname "")))

(dolist (dir (uiop/driver:subdirectories (pathname "~/work/snd/sfz/NorrfjardenChurch/")))
  (let ((dirname  (first (last (pathname-directory dir)))))
    (write-sfz dir dirname "")))
|#




