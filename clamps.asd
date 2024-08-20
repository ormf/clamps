;;; clamps.asd

(in-package :cl-user)

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
