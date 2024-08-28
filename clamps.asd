;;; clamps.asd

(in-package :cl-user)

(pushnew (merge-pathnames "local-projects/clamps/packages/svg-import-export/" ql:*quicklisp-home*)
      asdf:*central-registry*)

(asdf:defsystem #:clamps
  :description "Common Lisp Aided Music Production System"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi
	       #:cl-ppcre
               #:cl-refs
               #:incudine
               #:cm
               #:fomus
               #:ats-cuda)
  :components ((:file "load-packages")
               (:file "package")
               (:file "clamps-utils")
               (:file "clamps")
               ))


(defparameter *sfz-preset-path* (list (pathname "~/work/snd/sfz/")))
(defparameter *sfile-path* (list (pathname "~/work/snd/")))
(defparameter *sfz-preset-lookup* (make-hash-table))

(defun clamps (&rest args)
  "Start Clamps including the gui.

Besides starting the Gui the function also:

- Starts the osc responder for Inkscape.
- Starts the realtime engine calling #'rts.
- Creates groups and buses for incudine dsps (see the
Chapter <<clamps:General Incudine Setup>>.
- Starts the documentation acceptor for the searchable online doc
at /http://localhost:8282/overview/index.html/.

@Arguments
gui-root - ist the path where to put the /www/ subfolder for files
accessible by the gui (nicknamed /<clamps-gui-root>/ throughout
this dictionary).

open - is a flag indicating whether to open the #'clamps-base-url in a
browser window after starting the gui.

In the given path the following directories
will be created:

- /<clamps-gui-root>/www//
- /<clamps-gui-root>/www/svg//

file path for svg files used in the /<clamps-base-url>/svg-display/ page
of the Gui.

Any files which need to be accessible by the Gui have to be put
into the /<clamps-gui-root>/www// subdirectory with their filenames
relative to this directory.

@See-also
clamps-base-url
clamps-restart-gui
clamps-gui-root
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
    (apply #'clampscall :clamps-start args)))

(defun clamps-no-gui ()
  (clamps :open-gui nil))

(export '(*sfz-preset-lookup* *sfz-preset-path* *sfile-path* clamps clamps-no-gui) 'cl-user)
