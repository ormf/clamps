;;; 
;;; make-sfz.lisp
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
  (let ((alteration
          (if (= 3 (length str))
              (if (eql (aref str 1) #\#) 1 -1)
              0)))
    (+ 12
       alteration
       (elt
        '(0 2 4 5 7 9 11)
        (position (aref str 0)
                  '(#\C #\D #\E #\F #\G #\A #\B)))
       (* 12 (read-from-string (subseq str (1- (length str))))))))

(defun prepend-pitch-num (stream dir template)
  "create shell commands to prepend the pitch number in front of
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

(defun write-sfz (dir name template)
  (with-open-file (out (format nil "~a/~a.sfz" dir name)
                       :direction :output :if-exists :supersede)
    (let* ((seq
             (loop for file in (find-files-matching (format nil "~a/samples/" dir) template)
                   for name = (file-namestring file)
                   collect (list (read-from-string (subseq name 0 3)) name)))
           (keys (mapcar #'first seq))
           (lokeys (cons 0 (mapcar (lambda (x) (1+ x)) (butlast keys))))
           (hikeys (append (butlast keys) '(127))))
      (loop
        for lokey in lokeys
        for hikey in hikeys
        for (key name) in seq
        do (format out "<region>
sample=samples/~a
volume=6
lokey=~a
hikey=~a
tune=0
pitch_keycenter=~a~%~%" name lokey hikey key)))))


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

#|

 Beispiel:

;;; Zuvor darauf achten, dass von jeder Tonhöhe nur 1 Datei vorhanden
;;; ist!

(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-pp" "bassoboe-pp")
(write-sfz "~/work/snd/sfz/bassoboe" "bassoboe-f" "bassoboe-f")

|#
