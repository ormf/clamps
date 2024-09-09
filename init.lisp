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

(defparameter *sfz-preset-path* (list (pathname "~/work/snd/sfz/"))
  "List of directories to search recursively for /.sfz/ files.")
(defparameter *sfile-path* (list (pathname "~/work/snd/"))
  "List of directories to search recursively for soundfiles.")
(defparameter *sfz-preset-lookup* (make-hash-table))

(load (merge-pathnames ".clampsinit.lisp" (user-homedir-pathname))
      :if-does-not-exist nil)

(defun clamps (&key (gui-root "/tmp") (qsynth nil) (open-gui nil))
  "Start Clamps including the Gui. This function can be called from the
/:cl-user/ package.

Apart from starting the webserver for the Gui, the function also:

- Starts the OSC responder for Inkscape.
- Starts the realtime engine and sets up MIDI ports and receivers by calling <<rts>>.
- Creates groups and buses for incudine dsps (see the Chapter <<clamps:General Incudine Setup>>.
- Starts the documentation acceptor for the online doc at /http://localhost:8282/overview/index.html/.

The following directories will be created in the gui-root path :

- /<clamps-gui-root>/www//
- /<clamps-gui-root>/www/svg/

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
             :gui-root gui-root :qsynth qsynth :open-gui open-gui)))

(export '(*sfz-preset-lookup* *sfz-preset-path* *sfile-path* clamps clamps-no-gui) 'cl-user)
