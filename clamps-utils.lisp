;;; 
;;; clamps-utils.lisp
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

(in-package :clamps)

(defparameter %clamps-version% 1)

(defun clamps-version-number (&rest arg) arg %clamps-version%)

(defun clamps-version-name ()
  (format nil
          "~a.~a.~a"
          (ldb (byte 8 16) %clamps-version%)
          (ldb (byte 8 8) %clamps-version%)
          (ldb (byte 8 0) %clamps-version%)))

(defun clamps-version (&rest fmat)
  (cond ((null fmat)
         (format nil "Clamps ~a" (ou:system-version :clamps)))
        ((not (null (cdr fmat)))
         (error "clamps-version: more than one arg: ~s." fmat))
        ((eq (car fmat) ':number) %clamps-version%)
        ((eq (car fmat) ':string) (clamps-version-name))
        ((eq (car fmat) ':list)
         (list (ldb (byte 8 16) %clamps-version%)
               (ldb (byte 8 8) %clamps-version%)
               (ldb (byte 8 0) %clamps-version%)))
        (t (error "clamps-version: Bad format: ~s." (car fmat)))))

(defparameter *clamps-logo* t)

(defun clamps-logo ()
  "draw clamps logo on *standard-output* the nerdy way. Originally written
by Tobias Kunze. Some cleanup done by Orm Finnendahl."
  (if *clamps-logo*
      (let ((e "~%"))
        (format t e)
        (do ((v (make-string 15)) (y 0 (+ y 1)))
            ((= y 7) nil)
          (format t
                  (do ((x 0 (+ x 1)))
                      ((= x 15) (cond
                                  ((= y 2)
                                   (concatenate
                                    'string v " CLAMPS" e))
                                  ((= y 3)
                                   (concatenate
                                    'string v " Common Lisp Aided Music Production System" e))
                                  ((= y 4)
                                   (concatenate
                                    'string v " Version " (clamps-version-name) e))
                                  (t (concatenate 'string v e))))
                    (setf (elt v x)
                          (cond
                            ((<= 2 (- x y) 4) #\\) 
                            ((= (- x (- 4 (mod (+ 13 y) 15))) 1) #\/)
                            ((<= 1 y 5) #\-)
                            ((= (* (- x 6) (- y 3)) 15) #\/)
                            (:else #\SPACE))))))
        (format t e)))
  (values))

(defun clamps:idump (node)
  "Dump all active dsps of /node/ to the /incudine:*​logger-stream​*/
output.

@Arguments
node - The id of the node

@Note
If /(idump)/ doesn't create any output although dsps are running,
reset the logger-stream using <<reset-logger-stream>>.
"
  (unless incudine.util:*logger-stream*
    (reset-logger-stream))
  (dump (incudine:node node)))

(defun clamps:set-tempo (bpm)
  "Set the tempo in beats per minute for both, CM and Incudine.

@Arguments
bpm - Number of beats per minute.

@See-also
set-bpm
"  (setf cm:*tempo* bpm)
  (setf (bpm *tempo*) bpm))

(defparameter *clamps-doc-acceptor* (make-instance 'hunchentoot:easy-acceptor
        :port 8282
        :document-root (asdf:system-relative-pathname :clamps "doc/html/clamps-doc/")))

(defun start-doc-acceptor ()
  "Start the doc acceptor for online documentation. This is done
automatically on startup to make the clamps documentation
accessible at the URL /https://localhost:8282/.
"  (when (hunchentoot::acceptor-listen-socket *clamps-doc-acceptor*)
     (hunchentoot:stop *clamps-doc-acceptor*))  
  (hunchentoot:start *clamps-doc-acceptor*))

;;; (start-doc-acceptor)

(setf (fdefinition 'clamps::set-bpm) #'clamps:set-tempo)

;;; breakpoint versions of n-lin and n-exp

(defun n-lin-bp (x bp min max)
  (n-lin (apply #'interp x (flatten bp)) min max))

(defun n-exp-bp (x bp min max)
  (declare (float min max))
  (n-exp (apply #'interp x (flatten bp)) min max))

(defun plot-2d (seq)
  "Convenience wrapper around <<plot>>: A flat sequence of numbers is
interpreted as 2-d coordinate pairs.

@Examples
#+BEGIN_SRC lisp
(plot-2d '(2 1 4 3 6 10)) <=> (plot '((2 1) (4 3) (6 10)))
#+END_SRC
"
  (plot (ou:group seq 2))
  (values))

(defun plot-3d (seq)
  "Plot a flat sequence of coordinates by grouping the elements in 3."
  (plot (ou:group seq 3))
  (values))

(defvar *standard-pitch* 440.0
  "Tuning reference for /ftom/ and /mtof/ in Hz. Defaults to 440.

@Important-Note

Don't set this value directly! Rather use the <<set-standard-pitch>>
function which changes the standard pitch reference for the entire
/Clamps/ system.

@See-also
ftom
mtof
set-standard-pitch
")

(defun set-standard-pitch (freq)
  "Set the ∗​standard-pitch​∗ reference of Clamps to freq in Hz.

@Arguments
freq - Frequency of A4 in Hz.

@See-also
*standard-pitch*
"
  (setf *standard-pitch* (float freq 1.0))
  (setf oid::*standard-pitch* (float freq 1.0)))

#|

(defun ftom (f &key (tuning-base *standard-pitch*))
  "Convert frequency in Hz to pitch in Midicents.

@Arguments
freq - Frequency in Hz.
:tuning-base - Frequency of A4 in Hz.

@Examples

(ftom 440) ; => 69.0

(ftom 269.3) ; => 60.500526

(ftom 415 :tuning-base 415) ; => 69.0

@See-also
mtof
"
  (+ 69 (* 12 (log (/ f tuning-base ) 2))))

(defun mtof (midi-value &key (tuning-base *standard-pitch*))
  "Convert pitch in Midicts to frequency in Hz.

@Arguments
midi-value - Pitch in Midicents.
:tuning-base - Frequency of A4 in Hz.

@Examples
(mtof 69) ; => 440

(mtof 60.5) ; => 269.29178

(mtof 69 :tuning-base 415) ; => 415

@See-also
ftom
"
(* tuning-base (expt 2 (/ (- midi-value 69) 12))))
|#

(defun fr2ct (ratio)
       "Return the Midicents interval of /ratio/.

@Arguments
ratio - The frequency ratio of the interval.
@Examples
#+BEGIN_SRC lisp
(fr2ct 2) ;; => 12.0

(fr2ct 4/5) ;; => -3.863137

(fr2ct 3/2) ;; => 7.01955

(fr2ct 1/2) ;; => -12.0
#+END_SRC

@See-also
ct2fr
"
  (* 12 (log ratio 2)))

(defun ct2fr (ct)
  "Return the frequency ratio of the Midicents interval /cent/.

@Arguments
cent - The interval in Midicents.
@Examples
#+BEGIN_SRC lisp
(ct2fr 12) ;; => 2

(ct2fr 1) ;; => 1.0594631

(ct2fr 7) ;; => 1.4983071

(ct2fr -12) ;; => 1/2
#+END_SRC

@See-also
fr2ct
"
  (expt 2 (/ ct 12)))
