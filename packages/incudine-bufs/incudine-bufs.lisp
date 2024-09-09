;;; 
;;; incudine-bufs.lisp
;;;
;;; simple buffer registry to avoid duplicate loading of buffers.
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

(in-package :incudine-bufs)

(defparameter *buffers* (make-hash-table))
(defparameter *buffer-ids* (make-hash-table :test #'equal))
(defparameter *buffer-max-id* -1)
(defparameter *buffer-next-id* '())

;;; 

(defun get-sndfile-path (fname path)
  (let ((pname (pathname fname)))
    (if (and (uiop:absolute-pathname-p pname)
             (uiop:file-exists-p pname))
        pname
        (or (path-find-file (format nil "~a.~a"
                                    (pathname-name pname)
                                    (pathname-type pname))
                            path)
            (warn "couldn't find file ~S in path" fname)))))

#+linux
(defun path-find-file (fname path)
  (let ((fname (pathname fname)))
    (if (uiop:file-exists-p fname)
        (namestring fname)
        (loop
          for dir in path
          for result = (string-trim
                        '(#\NEWLINE)
                        (with-output-to-string (str)
                          (uiop:run-program (format nil "find ~a -name ~a" dir fname) :output str)))
          while (string= result "")
          finally (return (unless (string= result "") result))))))

#-linux
(defun path-find-file (fname path)
  (let ((fname (pathname fname)))
    (if (uiop:file-exists-p fname)
        (namestring fname)
        (loop
          for dir in path
          for result = (first (directory (format nil "~a/**/~a" dir fname)))
          until result
          finally (return result)))))

(defun buffer-id (buffer)
  "get index of buffer from registry."
  (gethash buffer *buffer-ids*))

(defun get-buffer (id)
  "get buffer from registry by index."
  (gethash id *buffers*))

(defun canonicalize-name (name)
  "remove leading directories from name"
  (let ((path (pathname name)))
    (format nil "~a.~a" (pathname-name path) (pathname-type path))))

(defun get-buffer-file (buffer)
  (canonicalize-name (buffer-file buffer)))



(defun find-buffer (name)
  "Find all buffers with a name being a full pathname or the
pathname-name of /name/.

@Arguments
name - String or Pathname denoting the buffer's filename.

@See-also
add-buffer
of-buffer-load
remove-buffer
remove-all-buffers
"
  (or (gethash name *buffers*)
      (let ((sfname (canonicalize-name name)))
        (loop for entry being the hash-key of *buffer-ids*
;;;              do (format t "~a, ~a~%" entry (if (typep entry 'incudine:buffer) (list (canonicalize-name (buffer-file entry)) sfname)))
              if (and (typep entry 'buffer) (string= (canonicalize-name (buffer-file entry)) sfname))
                collect entry into result
              finally (return (if (= (length result) 1) (first result) result))))))

;;; (loop for i below 10 if (evenp i) collect i into result finally (return result))

(defun add-buffer (buf)
  "Add buffer to registry.

@Arguments
buf - Incudine:buffer

@See-also
find-buffer
of-buffer-load
remove-buffer
remove-all-buffers
"
  (unless (gethash buf *buffer-ids*)
    (setf (gethash (or (pop *buffer-next-id*) (incf *buffer-max-id*)) *buffers*) buf)
    (setf (gethash buf *buffer-ids*) *buffer-max-id*)
    (setf (gethash (buffer-file buf) *buffers*) buf)
    (setf (gethash (pathname-name (buffer-file buf)) *buffers*) buf)
    buf))

(defun remove-buffer (buf)
  "Remove buffer from registry.

@Arguments
buf - Incudine:buffer

@See-also
add-buffer
find-buffer
of-buffer-load
remove-all-buffers
"
  (let ((id (buffer-id buf)))
    (unless (and
             id
             (remhash buf *buffer-ids*)
             (remhash (buffer-file buf) *buffers*)
             (if (remhash id *buffers*)
                 (push id *buffer-next-id*)))
      (warn "Can't remove buffer ~a: buf or id not found in databases!" buf))))

(defun remove-all-buffers ()
  "Remove all buffers from registry.

@See-also
add-buffer
find-buffer
of-buffer-load
remove-buffer
"
  (setf *buffer-ids* (make-hash-table :test #'equal))
  (setf *buffers* (make-hash-table))
  (setf *buffer-next-id* '())
  (setf *buffer-max-id* -1)
  nil)

;;; (remove-all-buffers)

(defun bufname= (buf file)
     "Compare file with the filename of buf. If buf is a list, compare
file to the filenames of all elements of list and return buf if any is
matching.

@Arguments
buf - Incudine:buffer
file - String denoting the file.

@See-also
add-buffer
find-buffer
of-buffer-load
remove-buffer
remove-all-buffers
"
     (cond
       ((null buf) nil)
       ((consp buf) (or (bufname= (first buf) file) (bufname= (rest buf) file)))
       (t (and (string= (format nil "~a" (buffer-file buf)) (format nil "~a" file)) buf))))

(defun of-buffer-load (file &key (path cl-user:*sfile-path*))
  "Load and register buffer from /file/ if not loaded already. Return
buffer.

@Arguments
file - Pathname or String denoting a soundfile.
:path - List of Pathnames or Strings to search for file.

@See-also
add-buffer
find-buffer
of-buffer-load
remove-buffer
remove-all-buffers
*sfile-path*
"
  (let ((buf (find-buffer file)))
    (if (bufname= buf file)
        buf
        (alexandria:if-let (fname (path-find-file file path))
          (add-buffer (buffer-load fname))
          (warn "couldn't find soundfile in sfile-path: ~A" file)))))

(setf (fdefinition 'ensure-buffer) #'of-buffer-load)

;;; (of-buffer-load "/home/orm/work/kompositionen/letzte-worte/snd/fl-s01-line01.wav")

