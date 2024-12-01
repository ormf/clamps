;;; 
;;; init-clamps.lisp
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

(in-package :cl-user)

(defparameter *sfz-file-path* (list (asdf:system-relative-pathname :clamps "extra/snd/sfz/"))
  "List of directories to search recursively for /.sfz/ files.

@See-also
add-sfz-preset
load-sfz-preset
")

(defparameter *sfile-path* (list (asdf:system-relative-pathname :ats-cuda "snd/"))
  "List of directories to search recursively for soundfiles.

@See-also
clamps-buffer-load
create-lsample
")

(defparameter *ats-file-path* (list (asdf:system-relative-pathname :ats-cuda "ats-data/"))
  "List of directories to search recursively for ats files.

@See-also
load-ats
")

(defparameter *sfz-preset-lookup* (make-hash-table))

;; Declaration for the init process only so that it is available in
;; ~/.clampsinit.lisp. In cl-sfz, #'add-sfz-preset is defined
;; seperately including documentation.

(defun add-sfz-preset (key fname)
  (setf (gethash key *sfz-preset-lookup*) fname))

(defun set-clamps-doc-root (url)
  (slynk:eval-in-emacs `(setq *common-music-doc-root* ,url)))

(defvar *clamps-doc-root* (concatenate 'string "file://" (namestring (merge-pathnames (asdf:system-relative-pathname :clamps "doc/html/clamps-doc/")))))

(load (merge-pathnames ".clampsinit.lisp" (user-homedir-pathname))
      :if-does-not-exist nil)

(defun clamps-image-start ()
  (setf *package* (find-package :cl-user)))

(defun clamps (&key (gui-base "/tmp") (qsynth nil) (open-gui nil))
  "Start Clamps including the Gui. This function can be called from the
/:cl-user/ package.

Apart from starting the webserver for the Gui, the function also:

- Starts the OSC responder for Inkscape.
- Starts the realtime engine and sets up MIDI ports and receivers by calling <<rts>>.
- Creates groups and buses for incudine dsps (see the Chapter <<clamps:General Incudine Setup>>.
- Starts the documentation acceptor for the online doc at /http://localhost:8282/overview/index.html/.

The following directories will be created in the gui-root path :

- /<clamps-gui-base>/www//
- /<clamps-gui-base>/www/svg/

The latter is the file path for svg files used in the
/<clamps-base-url>/svg-display/ page of the Gui.

Any files which need to be accessible by the Gui have to be put
into the /<clamps-gui-root>/www// subdirectory with their filenames
relative to this directory.

@Arguments
:gui-root - String or Pathname indicating where to put the /www/ subfolder
for files accessible by the gui (nicknamed /<clamps-gui-root>/).

:open-gui - Boolean indicating whether to open the /<clamps-base-url>/ in a
browser window after starting the gui.

:qsynth - Boolean indicating whether to start the Qsynth
softsynth (Linux only).

@See-also
clamps-base-url
clamps-restart-gui
clamps-start
clamps-gui-root
rts
"
  (flet ((clampscall (fn &rest args)
           (apply (find-symbol (string fn) :clamps) args))
         (cmvar (var)
           (symbol-value (find-symbol (string var) :cm))))
    (cl-user::set-clamps-doc-root cl-user::*clamps-doc-root*)
    (setf *package* (find-package :clamps))
    (setf *readtable* (cmvar :*cm-readtable*))
    ;; add slime readtable mapping...
    (let ((swank-pkg (find-package :swank))
          (slynk-pkg (find-package :slynk)))
      (when swank-pkg
        (let ((sym (intern (symbol-name :*readtable-alist*) swank-pkg)))
          (setf (symbol-value sym)
                (cons (cons (symbol-name :cm)
                            (cmvar :*cm-readtable*))
                      (symbol-value sym))))
        (when slynk-pkg
          (let ((sym (intern (symbol-name :*readtable-alist*) slynk-pkg)))
            (setf (symbol-value sym)
                  (cons (cons (symbol-name :cm)
                              (cmvar :*cm-readtable*))
                        (symbol-value sym)))))))
    (funcall #'clampscall :clamps-start
             :gui-base gui-base :qsynth qsynth :open-gui open-gui)))

(export '(*sfz-preset-lookup* *sfz-file-path* *sfile-path* *ats-file-path* set-clamps-doc-root *clamps-doc-root* clamps clamps-no-gui clamps-image-start) 'cl-user)
