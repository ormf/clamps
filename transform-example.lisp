;;; 
;;; transform-example.lisp
;;;
;;; transform a file with multiple instruments, exported from
;;; polyphone into single folders with their .sfz files and samples to
;;; be used individually.
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

(defun sanitize-filename (name)
  (string-downcase (remove
                    #\'
                    (remove #\& (substitute #\- #\)
                                            (substitute #\- #\(
                                                        (substitute #\- #\SPACE name )))))))

(defun rewrite-sfz (src dest)
  (let ((srcdir (path:dirname src))
        (destdir (path:dirname dest)))
    (with-open-file (out dest :direction :output :if-exists :supersede)
      (with-open-file (in src)
        (loop
          for line = (read-line in nil nil)
          while line
          do (if (and (> (length line) 8) (equal (subseq line 0 7) "sample="))
                 (let* ((sample-file (cl-ppcre:regex-replace "../" (subseq (substitute #\/ #\\ line) 7) ""))
                        (dest-sample-file (sanitize-filename sample-file)))
                   (format out "sample=~a~%" dest-sample-file)
                   (uiop:run-program (format nil "cp \"~a/~a\" ~a/~a" srcdir sample-file destdir dest-sample-file)))
                 (format out "~a~%" line)))))))

(defun transform-sfz (file destdir-toplevel)
  (let* ((src-file (sanitize-filename (filename-name file)))
         (dest-dir (merge-pathnames (format nil "~a/" src-file) destdir-toplevel))
         (sfz-name (format nil "~a.sfz" src-file)))
    (uiop:run-program (format nil "mkdir -p ~a/samples" dest-dir))
    (rewrite-sfz file (merge-pathnames sfz-name dest-dir))))

#|

;;; Example invocations (call "mkdir -p /tmp/sfz/new" first and put
;;; your folder with the sfz files and samples into /tmp/sfz):

(let ((srcdir (pathname "/tmp/sfz/freepats-general-midi/")))
  (mapc (lambda (file) (transform-sfz file (pathname "/tmp/sfz/new/")))
          (uiop:directory-files srcdir "*.sfz")))

(let ((srcdir (pathname "/tmp/sfz/petit-italien/")))
  (mapc (lambda (file) (transform-sfz file (pathname "/tmp/sfz/new/")))
        (uiop:directory-files srcdir "*.sfz")))

(let ((srcdir (pathname "/tmp/sfz/FluidR3_GM-orm/")))
  (mapc (lambda (file) (transform-sfz file (pathname "/tmp/sfz/new/")))
          (uiop:directory-files srcdir "*.sfz")))

(let ((srcdir (pathname "/tmp/sfz/FluidR3_GM-orm/")))
  (uiop:directory-files srcdir "*.sfz"))

|#
