;;; clamps.asd

(asdf:defsystem #:clamps
  :description "Common Lisp Aided Music Production System"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "Gnu Public license, version 2.0 or later."
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre
               #:cl-refs
               #:orm-utils
               #:incudine
               #:cm
               #:fomus
               #:ats-cuda)
  :components ((:file "load-packages")
               (:file "package")
               (:file "clamps-utils")
               (:file "clamps")))

(in-package :cl-user)

(defun clamps (&rest systems)
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
    (let (#-sbcl (*trace-output* nil))
      (dolist (s systems) (use-system s :verbose nil)))
    (clampscall :start-clamps)))

(export '(clamps) 'cl-user)
