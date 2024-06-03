(in-package :cl-user)

(export '(*sfz-preset-lookup* *sfz-preset-path* *sfile-path*) 'cl-user)

(defparameter *sfz-preset-path* (list (pathname "~/next-generation-24/snd/sfz/")))
(defparameter *sfile-path* (list (pathname "~/next-generation-24/snd/")))
(defparameter *sfz-preset-lookup* (make-hash-table))

(let ((sfz-assoc
        '(:flute-nv "000_Flute-nv.sfz"
          :yamaha-grand-piano "yamaha-grand-piano.sfz")))
  (loop for (sym fname) on sfz-assoc by #'cddr
        do (progn
;;;             (format t "~&registering ~S at ~S~%" sym fname)
             (setf (gethash sym *sfz-preset-lookup*) fname))))
