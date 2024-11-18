;;; Personal initialization file for common music/clamps, setting up
;;; paths for soundfiles and sfz files and associations between
;;; keywords and sfz files.
;;;
;;; Put this file into your home directory (at "$HOME/.cminit.lisp")
;;; and adjust as needed. It will be executed each time, Clamps ist
;;; loaded into lisp.

(in-package :cl-user)

;;; The following paths are the defaults, where sfz and soundfiles are
;;; located in the clamps repository. In order to be able to pull
;;; updates of clamps later on it is advisable to add your own sfz
;;; files and soundfiles into a folder of your choice and push this
;;; folder as a pathname to the *sfz-file-path* and *sfile-path*
;;; variables. All directories listed in *sfile-path* or
;;; *sfz-preset-path* will get searched recursively for soundfiles or
;;; sfz files respectively. Additional sfz associations between the
;;; preset keyword and an sfz file should be added using the
;;; "add-sfz-assoc" function.
;;;
;;; Example (you can add the example code at the end of this file to
;;; load it each time, Clamps is loaded):
;;;
;;; Soundfiles in "$HOME/snd", an sfz file of the name "clarinet.sfz"
;;; in "$HOME/snd/sfz" :
;;;
;;; (pushnew "~/snd" *sfile-path*)
;;; (pushnew "~/snd/sfz" *sfz-file-path*)
;;; (add-sfz-assoc :clarinet "clarinet.sfz")
;;;
;;; After this, (sprout (new sfz :time 0 :preset :clarinet)) should
;;; use the clarinet.sfz file to output a sound of keynum 60 for 0.5
;;; seconds.
;;;
;;; Loading ats files use the path *ats-file-path*. Push into this
;;; list to add other pathnames:
;;;
;;; (pushnew (pathname "~/snd/ats") *ats-file-path*))
;;;

(let ((sfz-assoc
        '(:flute-nv "000_Flute-nv.sfz"
          :yamaha-grand-piano "yamaha-grand-piano.sfz")))
  (format t "~&registering ~a sfz-files" (/ (length sfz-assoc) 2))
  (loop for (sym fname) on sfz-assoc by #'cddr
        do (progn
             (format t ".")
             (add-sfz-preset sym fname fname)))
  (format t "done~%"))

