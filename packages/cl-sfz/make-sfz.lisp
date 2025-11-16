;;; 
;;; make-sfz.lisp
;;;
;;; utilities to help creating sfz files from sample names.
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

(defun parse-pitch (str)
  "Return MIDI keynum of /str/ of the form \"C4\" \"C#5\" \"Ab3\". The
string can be in upper or lower case letters. \"C4\" returns 60."
  (let ((alteration
          (if (= 3 (length str))
              (if (eql (aref str 1) #\#) 1 -1)
              0)))
    (+ 12
       alteration
       (getf
        '(#\C 0 #\D 2 #\E 4 #\F 5 #\G 7 #\A 9 #\B 11
          #\c 0 #\d 2 #\e 4 #\f 5 #\g 7 #\a 9 #\b 11)
        (aref str 0))
       (* 12 (read-from-string (subseq str (1- (length str))))))))

(defun prepend-pitch-num (stream dir template)
  "Create shell commands to prepend the pitch number in front of
sample files by parsing the pitch name."
  (loop for file in (uiop:directory-files dir)
        for name = (file-namestring file)
        for pitchname = (and (cl-ppcre:scan template name)
                             (cl-ppcre:regex-replace template name "\\1"))
        if pitchname collect (format stream "mv ~a/~a ~a/~3,'0d-~a"
                                     dir name dir
                                     (parse-pitch pitchname)
                                     name)))

;;; (prepend-pitch-num "~/work/snd/sfz/trombone/trb-ord-mf/samples/" "Tbn-ord-(.*)-mf.aif$")

(defun rename-samples (dir template)
  (uiop:run-program (format nil "mkdir -p ~a/samples/old" dir))
  (uiop:run-program (format nil "cp ~a/samples/*.aif ~a/samples/old/" dir dir))
  (map nil #'uiop:run-program
       (loop for string in '("1c" "2c" "3c" "4c")
             append (loop for dynamic in '("ff" "mf" "pp")
                          append (prepend-pitch-num nil
                                                    (format nil "~a/samples/" dir)
                                                    (format nil template dynamic string))))))

(defun find-files-matching (dir template)
  (remove-if-not (lambda (file) (cl-ppcre:scan template (file-namestring file)))
                 (uiop:directory-files dir)))

#|

(dotimes (c 6)
  (let* ((name (format nil "guitar-pizz-orm-c~a" (1+ c)))
         (dir (format nil "/home/orm/work/snd/sfz/guitar-pizz-orm/~a/" name))) 
    (with-open-file (out (format nil "~a/~a.sfz" dir name)
                         :direction :output :if-exists :supersede)
      (let* ((sampledir (format nil "~a/samples/" dir))
             (files (uiop:directory-files sampledir))
             (all-keys (mapcar #'get-guitar-pitch files))
             (keys (sort (remove-duplicates all-keys) #'<))
             (lokeys (cons 0 (mapcar (lambda (x) (1+ x)) (butlast keys))))
             (hikeys (append (butlast keys) '(127)))
             (range-assoc (mapcar #'list keys lokeys hikeys)))
        (loop
          for key in all-keys
          for lokey = (second (assoc key range-assoc))
          for hikey = (third (assoc key range-assoc))
          for file in files
          for name = (file-namestring file)
          do (format out "<region>
sample=samples/~a
volume=6
lokey=~a
hikey=~a
tune=0
pitch_keycenter=~a~%~%" name lokey hikey key))))))

|#

(defun write-sfz (dir name template &key (volume 0))
  "Generate and write an sfz file using all samples in the samples subdir
of /dir/ with /name/ using /template/ with optional /volume/ in
dB. The names of samplefiles have to start with 3 digits denoting the
midi keynum.

@Arguments
dir - String or Pathname denoting the parent dir of the soundfont to create.
name - String denoting the basename of the sfz file.
template - String denoting a template for samples to match
:volume - Number denoting the volume in dB (0dB = unit gain) in each sample region of the sfz.

@Example

;;; generate a sfz file named ~/work/snd/sfz/bd/bd.sfz from all samples in the
;;; ~/work/snd/sfz/bd/samples/ subdirectory named 001-bd.wav,
;;; 002-bd.wav,...

(write-sfz \"~/work/snd/sfz/bd/\" \"bd\" \"bd\")

"
  (with-open-file (out (format nil "~a/~a.sfz" dir name)
                       :direction :output :if-exists :supersede)
    (let* ((seq
             (loop for file in (find-files-matching (format nil "~a/samples/" dir) template)
                   for name = (file-namestring file)
                   collect (list (read-from-string (subseq name 0 3)) name)))
           (all-keys (mapcar #'first seq))
           (keys (sort (remove-duplicates all-keys) #'<))
           (lokeys (cons 0 (mapcar (lambda (x) (1+ x)) (butlast keys))))
           (hikeys (append (butlast keys) '(127)))
           (range-assoc (mapcar #'list keys lokeys hikeys)))
      (loop
        for (key name) in seq
        for lokey = (second (assoc key range-assoc))
        for hikey = (third (assoc key range-assoc))
        do (format out "<region>
sample=samples/~a
volume=~a
lokey=~a
hikey=~a
tune=0
pitch_keycenter=~a~%~%" name volume lokey hikey key)))))


(defun normalize-files (dir)
  (dolist (file (uiop:directory-files dir "*.sfz"))
    (uiop:run-program (format nil "sed -i.bup 's/volume=-*[0-9]\\+/volume=0/g' ~a" file)))
  (uiop:run-program (format nil "mkdir -p ~a/samples/unnormalized" dir))
  (uiop:run-program (format nil "mv ~a/samples/*.wav ~a/samples/unnormalized" dir dir))
  (dolist (file (uiop:directory-files (format nil "~a/samples/unnormalized/" dir) "*.wav"))
    (uiop:run-program (format nil "sox --norm=0 ~a ~a" file (format nil "~a/samples/~a" dir (file-namestring file)))))

  (loop
  for instr in '("violin" "viola" "violoncello" "doublebass")
  do (loop
       for type in '("ordinario" "nonvib")
       do (progn
            (dolist (file (uiop:directory-files (format nil "/home/orm/work/snd/sfz/~a/~a/" instr type) "*.sfz"))
              (uiop:run-program (format nil "sed -i.bup 's/volume=-*[0-9]\\+/volume=0/g' ~a" file)))
            (normalize-files (format nil "/home/orm/work/snd/sfz/~a/~a/" instr type))))))

(defparameter *sol-pitch-template* "-*([^-]+).+$")

(defun sol-parse-pitch (filename template)
  (parse-pitch (cl-ppcre:regex-replace *sol-pitch-template* (subseq filename (length template)) "\\1")))

(defun sol-prepend-pitch (filename template)
  (format nil "~3,'0d-~a" (sol-parse-pitch filename template) filename))

(defun sol->sfz (dir name template)
  (uiop:run-program (format nil "mkdir -p ~a/samples/orig/" dir) :ignore-error-status t)
  (dolist (file (find-files-matching (format nil "~a/samples/" dir) template))
    (uiop:run-program (format nil "mv ~a ~a/samples/orig/" file dir)))
  (dolist (file (find-files-matching (format nil "~a/samples/orig/" dir) template))
    (uiop:run-program (format nil "cp ~a ~a/samples/~a" file dir
                              (sol-prepend-pitch (file-namestring file) template))))
  (write-sfz dir name template))

#|

 Beispiel:

;;; Zuvor darauf achten, dass von jeder Tonhöhe nur 1 Datei vorhanden
;;; ist!

(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-pp" "bassoboe-pp")
(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-f" "bassoboe-f")

(find-files-matching (format nil "~a/samples/orig" dir) template)


;;; Example call: first copy the sol directory in question into the locations below.

(loop for (dir template) in '(("~/work/snd/sfz/violin/artificial-harmonic/" "Vn-art-harm")
                              ("~/work/snd/sfz/viola/artificial-harmonic/" "Va-art-harm")
                              ("~/work/snd/sfz/violoncello/artificial-harmonic/" "Vc-art-harm")
                              ("~/work/snd/sfz/doublebass/artificial-harmonic/" "Cb-art-harm"))
      do (sol->sfz dir (string-downcase template) template))

(loop for (dir template) in '(("~/work/snd/sfz/violin/artificial-harmonic/" "Vn-art-harm")
                              ("~/work/snd/sfz/viola/artificial-harmonic/" "Va-art-harm")
                              ("~/work/snd/sfz/violoncello/artificial-harmonic/" "Vc-art-harm")
                              ("~/work/snd/sfz/doublebass/artificial-harmonic/" "Cb-art-harm"))
      do (sol->sfz dir (string-downcase template) template))

(let ((base "vn-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/violin/artificial-harmonic/" (format nil "~a-~a" base string) string)))
(let ((base "va-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/viola/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(let ((base "vc-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/violoncello/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(let ((base "cb-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/doublebass/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(dolist (dir '("~/work/snd/sfz/violin/artificial-harmonic/"
               "~/work/snd/sfz/viola/artificial-harmonic/"
               "~/work/snd/sfz/violoncello/artificial-harmonic/"
               "~/work/snd/sfz/doublebass/artificial-harmonic/"))
  (dolist (template '("1c" "2c" "3c" "4c"))
    (sol->sfz dir (string-downcase template) template)))



(let ((base "vn-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/violin/artificial-harmonic/" (format nil "~a-~a" base string) string)))
(let ((base "va-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/viola/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(let ((base "vc-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/violoncello/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(let ((base "cb-art-harm"))
  (dolist (string '("1c" "2c" "3c" "4c"))
    (write-sfz "~/work/snd/sfz/doublebass/artificial-harmonic/" (format nil "~a-~a" base string) string)))

(dolist (dir '("~/work/snd/sfz/violin/artificial-harmonic/"
               "~/work/snd/sfz/viola/artificial-harmonic/"
               "~/work/snd/sfz/violoncello/artificial-harmonic/"
               "~/work/snd/sfz/doublebass/artificial-harmonic/"))
  (dolist (template '("1c" "2c" "3c" "4c"))
    (sol->sfz dir (string-downcase template) template)))


(loop for (dir template) in '(("~/work/snd/sfz/violin/ponticello/" "Vn-pont")
                              ("~/work/snd/sfz/viola/ponticello/" "Va-pont")
                              ("~/work/snd/sfz/violoncello/ponticello/" "Vc-pont")
                              ("~/work/snd/sfz/doublebass/ponticello/" "Cb-pont"))
      do (sol->sfz dir (string-downcase template) template))

|#
