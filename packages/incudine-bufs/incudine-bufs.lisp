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
#|
(defun get-sndfile-path (fname path)
  (let ((pname (pathname fname)))
    (if (and (uiop:absolute-pathname-p pname)
             (uiop:file-exists-p pname))
        pname
        (or (path-find-file (file-namestring pname)
                            path)
            (warn "couldn't find file ~S in path" fname)))))
|#

(defun path-find-file (fname path)
  "Return the full pathname of the first occurence of fname in path.

@Arguments
fname - String or Pathname of file.
path - List of paths to search.
"
  (let ((fname (pathname fname)))
    (if (uiop:file-exists-p fname)
        (namestring fname)
        (loop
          for dir in path
          for result = (string-trim
                        '(#\NEWLINE)
                        (with-output-to-string (str)
                          #-darwin
                          (uiop:run-program (format nil "find ~a -name ~a -print -quit" dir fname) :output str)
                          #+darwin
                          (uiop:run-program (format nil "find ~a -name ~a -print -exit" dir fname) :output str)))
          while (string= result "")
          finally (return (unless (string= result "") (pathname result)))))))

#|
(defun path-find-file (fname path)
  (let ((fname (pathname fname)))
    (if (uiop:file-exists-p fname)
        (namestring fname)
        (loop
          for dir in path
          for result = (first (directory (format nil "~a/**/~a" dir fname)))
          until result
          finally (return result)))))
|#

(defun buffer-id (buffer)
  "Return index of /buffer/ from registry.

@Arguments
buffer - Incudine buffer.

@See-also
add-buffer
clamps-buffer-load
find-buffer
get-buffer-file
list-buffers
remove-buffer
remove-all-buffers
"
  (gethash buffer *buffer-ids*))

;;; (namestring (pathname "/tmp/test.wav"))
;;; (file-namestring (pathname "/tmp/test.wav"))
;;; (directory-namestring (pathname "/tmp/test.wav"))

(defun canonicalize-name (name)
  "Return filename of buffer"
  (let ((path (pathname name)))
    (file-namestring path)))

(defun get-buffer-file (buffer)
  "Return the file-namestring of /buffer/.

@Arguments
buffer - Incudine buffer.

@See-also
add-buffer
buffer-id
find-buffer
list-buffers
remove-buffer
remove-all-buffers
"
  (canonicalize-name (buffer-file buffer)))

(defun find-buffer (ref)
  "Return registered buffer with /ref/ either being a full pathname, the
pathname-name or an integer id.

@Arguments
ref - Integer denoting id of buffer or String or Pathname denoting the buffer's filename.

@See-also
add-buffer
buffer-id
clamps-buffer-load
get-buffer-file
list-buffers
remove-buffer
remove-all-buffers
"
  (gethash ref *buffers*))

;;; (loop for i below 10 if (evenp i) collect i into result finally (return result))

(defun add-buffer (buf)
  "Add buffer to registry.

@Arguments
buf - Incudine buffer.

@See-also
buffer-id
clamps-buffer-load
find-buffer
get-buffer-file
list-buffers
remove-buffer
remove-all-buffers
"
  (declare (type buffer buf))
  (unless (gethash buf *buffer-ids*)
    (setf (gethash (or (pop *buffer-next-id*) (incf *buffer-max-id*)) *buffers*) buf)
    (setf (gethash buf *buffer-ids*) *buffer-max-id*)
    (setf (gethash (buffer-file buf) *buffers*) buf)
    (setf (gethash (pathname-name (buffer-file buf)) *buffers*) buf)
    buf))

(defun remove-buffer (ref)
  "Remove buffer from registry.

@Arguments
buf - Incudine:buffer, Integer denoting buffer id or filename of buffer.

@See-also
add-buffer
buffer-id
clamps-buffer-load
find-buffer
get-buffer-file
list-buffers
remove-all-buffers
"
  (let* ((buf (find-buffer ref))
         (id (gethash buf *buffer-ids*)))
    (unless (and
             id
             (remhash buf *buffer-ids*)
             (remhash (buffer-file buf) *buffers*)
             (if (remhash id *buffers*)
                 (pushnew id *buffer-next-id*)))
      (warn "Can't remove buffer ~a: buf or id not found in databases!" buf))))

(defun remove-all-buffers ()
  "Remove all buffers from registry.

@See-also
add-buffer
buffer-id
clamps-buffer-load
find-buffer
get-buffer-file
list-buffers
remove-buffer
"
  (setf *buffer-ids* (make-hash-table :test #'equal))
  (setf *buffers* (make-hash-table))
  (setf *buffer-next-id* '())
  (setf *buffer-max-id* -1)
  nil)

;;; (remove-all-buffers)

(defun bufname= (buf file)
     "Compare /file/ with the filename of /buf/. If buf is a list, compare
file to the filenames of all elements of list and return buf if any is
matching.

@Arguments
buf - Incudine:buffer
file - String denoting the file.

@See-also
add-buffer
buffer-id
clamps-buffer-load
find-buffer
get-buffer-file
list-buffers
remove-buffer
remove-all-buffers
"
     (cond
       ((null buf) nil)
       ((consp buf) (or (bufname= (first buf) file) (bufname= (rest buf) file)))
       (t (and (string= (format nil "~a" (buffer-file buf)) (format nil "~a" file)) buf))))

(defun clamps-buffer-load (file &key (path cl-user:*sfile-path*))
  "Load and register buffer from /file/ if not loaded already. Return
buffer.

@Arguments
file - Pathname or String denoting a soundfile.
:path - List of Pathnames or Strings to search for file.

@See-also
add-buffer
buffer-id
find-buffer
get-buffer-file
list-buffers
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

(defun list-buffers ()
  "Return a list (/buffer-id/ /buffer-name/) for each entry in buffer
registry.

@See-also
add-buffer
buffer-id
find-buffer
get-buffer-file
remove-buffer
remove-all-buffers
"
  (sort
   (loop
     for k being the hash-keys of *buffers*
     if (stringp k)
       collect (list (buffer-id (find-buffer k)) k))
   #'< :key #'first))

(setf (fdefinition 'ensure-buffer) #'clamps-buffer-load)

;;; (clamps-buffer-load "/home/orm/work/kompositionen/letzte-worte/snd/fl-s01-line01.wav")
