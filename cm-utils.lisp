;;; cm-utils.lisp
;;;
;;; Copyright (c) 2017 Orm Finnendahl
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(in-package #:cm)

;;(defparameter *local-time* 0)

(defun testmidi () (output (new midi)))

(defun jbmf ()
  "midi-rendering of \"Jesu bleibet meine Freude of J.S.Bach (Hommage
  to the Video Series of \"Structures and Interpretation of Computer
  Programs\")"
  (output (new midi-program-change :program 24))
  (sprout
   (let ((tscale 0.3))
     (process
       for event in
       '((1 43 3) (1 67 1) (1 69 1) (0 55 3) (0 71 1) (0 67 2) (1 62 3) (1 74 1)
         (0 72 1) (1 66 1) (0 67 2) (0 72 1) (0 52 3) (1 64 3) (1 76 1) (0 69 1)
         (1 74 1) (0 71 2) (0 47 3) (0 74 1) (1 67 3) (1 79 1) (0 78 1) (1 69 1)
         (0 79 1) (0 71 2) (0 64 3) (1 52 3) (1 74 1) (0 67 1) (1 71 1) (0 67 1)
         (0 64 2) (0 40 3) (1 59 3) (1 69 1) (0 62 1) (1 71 1) (0 64 2) (0 72 1)
         (0 45 3) (1 57 3) (1 74 1) (0 76 1) (1 66 1) (0 62 2) (0 47 3) (0 67 3)
         (1 74 1) (1 72 1) (0 62 1) (1 71 1) (0 64 3) (0 48 3) (0 69 1) (1 60 2)
         (1 71 1) (0 59 1) (1 67 1) (0 57 3) (0 60 2) (0 66 1) (1 50 3) (1 67 1)
         (0 62 1) (1 69 1) (0 69 2) (0 54 3) (0 57 3) (1 62 1) (1 66 1) (0 69 1)
         (1 67 1) (0 72 1) (0 69 2) (0 50 3) (1 66 2) (1 71 1) (0 66 1) (1 69 1)
         (0 71 1) (0 55 3) (0 67 1) (1 62 2) (1 67 1) (0 69 1) (1 66 1) (0 71 1)
         (0 67 2) (1 55 3) (1 74 1) (0 72 1) (1 66 1) (0 72 1) (0 64 3) (0 48 3)
         (1 67 2) (1 76 1) (0 74 1) (1 69 1) (0 71 2) (0 74 1) (0 67 3) (1 47 3)
         (1 79 1) (0 78 1) (1 69 1) (0 52 3) (0 79 1) (0 71 2) (1 64 3) (1 74 1)
         (0 71 1) (1 67 1) (0 64 2) (0 67 1) (0 50 3) (1 59 3) (1 69 1) (0 71 1)
         (1 67 1) (0 69 3) (0 60 2) (0 64 1) (1 48 3) (1 74 1) (0 72 1) (1 66 1)
         (0 71 1) (0 67 2) (0 49 3) (1 64 1.95) (1 69 1) (0 67 1) (1 64 1) (0 50 3)
         (0 62 1) (1 57 2) (1 67 1) (0 60 1) (1 66 1) (0 43 3) (0 62 2) (0 67 1)
         (1 59 3) (1 71 1) (0 74 1) (1 67 1) (0 79 1) (1 71 1.95) (1 74 1) (1 71 1)
         (1 67 3) )
       do (output (new midi :time (now)
                       :keynum (second event)
                       :amplitude 0.3
                       :duration (* tscale 1 (third event))))
       wait (* tscale (first event))
       finally (output (new midi-program-change :program 0))))))

;;; extension for a more general process type than in cm

(defmacro rt-wait (time &optional (yield t))
  `(progn
     (cm:wait ,time)
;;     (incf *local-time* ,time)
     (cl-coroutine:yield ,yield)))

(defmacro rt-sprout (s-expr &key (at))
  `(sprout ,s-expr :at (or ,at (now))))

(defmacro rt-proc (&body body)
  (alexandria:with-gensyms (name)
    `(progn
       (cl-coroutine:defcoroutine ,name () ,@body)
       (cl-coroutine:make-coroutine ',name))))

(defmacro rt-sub (&rest rest)
  (alexandria:with-gensyms (fn)
    `(let ((,fn (eval ,@rest)))
       (loop
          while (funcall ,fn) 
          do (cl-coroutine:yield t)))))

;;; channel-tuning utility

(defmacro make-mt-stream (symbol-name midi-out-stream chan-tuning)
  "Define, open and initialize a microtonal midistream. The name of
the stream and an already initialized midi-port-stream has to be
supplied and gets interned as a parameter."
  `(progn
     (defparameter ,symbol-name
       (new incudine-stream
         :name (string-trim '(#\*) (format nil "~a" ',symbol-name))
         :output ,midi-out-stream))
     (open-io (apply #'init-io ,symbol-name
                     `(:channel-tuning ,,chan-tuning)) :output ,midi-out-stream)
     (initialize-io ,symbol-name)
     (values ',symbol-name)))

(defun drunk-traverse (seq &key (weight 0.5))
  "shuffle an ordered list slightly by randomly swapping the positions
of neighboring elements."
  (cond ((null seq) nil)
        ((null (cdr seq)) seq)
        (:else (if (< (random 1.0) weight)
                   (cons (second seq) (drunk-traverse
                                       (cons (first seq)
                                             (nthcdr 2 seq)) :weight weight))
                   (cons (first seq) (drunk-traverse (rest seq) :weight weight))))))


;;; (drunk-traverse '(1 2 3 4 5 6 7) :weight 0.5)

(defmacro new-permutation (&key of permutation)
  `(if (/= (length ,of) (length ,permutation))
       (error "seq length (~a) and permutation length (~a) not identical"
              (length ,of) (length ,permutation))
       (new rewrite :of `,(loop 
                             for x in ,of
                             for y in ,permutation 
                             collect (list x :-> (elt ,of y)))
            :initially ,of)))

(defun r-interpl (min max &key (base 1))
  "random value between [min..max] with variable base. Default is
linear (base=1)."
  (interpl (random 1.0) (list 0 min 1 max) :base base))




;; (defparameter pat1 nil)
;; (setf pat1 (new-permutation :of '(A B C D E) :permutation '(3 4 1 2 0)))
;; (next pat1 #t) -> (A B C D E)
;; (next pat1 #t) -> (D E B C A)
;; (next pat1 #t) -> (C A E B D)
;; (next pat1 #t) -> (B D A E C)
;; (next pat1 #t) -> (E C D A B)
;; (next pat1 #t) -> (A B C D E)

(defun chord-derive (seq &key (level '(1)))
  "rotation of seq transposed to the first note of the original seq
developed/used by Boulez."
  (let ((seq (mapcar #'keynum seq)))
    (loop
      for l in level
      for transposition = (- (first seq) (elt seq l))
      collect (mapcar (lambda (pitch) (note (+ pitch transposition)))
                      (ou:rotate seq l)))))

;;; (chord-derive '(gs3 c5 f5 ef4 bf2 cs5) :level '(1 2))

(defun display (seqs &key (file "/tmp/test.ly"))
  "display a (seq of) seq of notes in lilypond."
  (let ((offs 0))
    (events
     (loop
       for seq in seqs
       append (loop
                for evt in seq
                collect (new midi :time offs :keynum (keynum evt))
                do (incf offs 0.5)))
     file
     :global (loop
               for offs = 0 then (+ offs (/ (length seq) 2))
               for seq in seqs
               collect (let ((tsig (/ (length seq) 8)))
                         (new fomus:timesig :off offs :time (list (numerator tsig)
                                                                  (denominator tsig))))))))

;;; (display (chord-derive '(gs3 c5 f5 ef4 bf2 cs5) :level '(1 2)))

(defun play-midi (seqs &key (tempo 60))
  "display a (seq of) seq of notes in lilypond."
  (let ((offs 0)
        (dtime (float (/ 60 tempo))))
    (sprout
     (loop
       for seq in seqs
       append (loop
                for evt in seq
                collect (new midi :time offs :keynum (keynum evt) :duration dtime)
                do (incf offs dtime))))))
(defun play-svg (file &key (region '(0 nil)) (tscale 0.25))
  (sprout
   (destructuring-bind (start end) region
     (loop for evt in (subobjects (import-events file :x-scale 1/8))
           append (if (and (>= (sv evt :time) (* 4 start))
                           (or (null end) (< (sv evt :time) (* 4 end))))
                      (list (progn
                              (setf (sv evt :time) (* tscale (+ (sv evt :time) (* -4 start))))
                              (setf (sv evt :duration) (* tscale (+ (sv evt :duration))))
                              evt)))))
   :to *rts-out*))

(defun incudine.scratch::node-free-unprotected ()
  (incudine:free (incudine:node 0))
  (dotimes (chan 4)
    (output (new midi-control-change :controller +all-notes-off+ :value 0 :channel chan)))
  :interrupted)

(defun calc-dur (min max orig-dur)
  "calculate effective duration of exponential function from minspeed,
maxspeed and original duration."
  (/ (* orig-dur (log (/ max min)))
     (- max min)))

(defun vstime->time-fn (min max end-time)
  "return a function calculating e.g. the sample-pos in a buffer at a
given time with given minspeed, maxspeed and end-time (= bufferlength)."
  (let* ((a (/ max min))
	 (b (/ end-time (- a 1)))
	 (dur (calc-dur min max end-time)))
    (format t "f(x) = ~a * (~a^x - 1)~%" b a)
    (lambda (time) (- (* b (expt a (/ time dur))) b))))

;;; (calc-dur 0.11552454 1.8483926 5000) -> 8000

;;; (calc-dur 1 8 10000)

;;; (funcall (vstime->time-fn 0.11552454 1.8483926 5000) 8000) => 5000.002

(defun time->vstime-fn (min max end-time)
  "return a function calculating the real time of a sample in the buffer with varispeed
applied. This is the inverse function of the varispeed function."
  (let* ((dur (calc-dur min max end-time))
	 (a (/ max min)))
    (lambda (buff-pos)
      (* dur (log (+ (* (/ buff-pos end-time) (- a 1)) 1) a)))))

(defun vstime->speed-fn (min max end-time)
  (let* ((a (/ max min)))
    (if (< (abs (log a)) 0.01)
	;; linear case:
	(let* ((avg-speed (/ (+ min max) 2))
	       (dur (/ end-time avg-speed))
	       (fac (/ (- max min) dur)))
	  (lambda (time)
	    (+ min (* time fac))))
	;; exponential case:
	(let* ((dur (calc-dur min max end-time))
	       (b (/ end-time (- a 1))))
	  (lambda (time)
	    (* b (/ (log a) dur) (expt a (/ time dur))))))))

(defun time->speed-fn (min max end-time)
  (let* ((a (/ max min)))
    (if (< (abs (log a)) 0.01)
	;; linear case:
	(let* ((avg-speed (/ (+ min max) 2))
	       (dur (/ end-time avg-speed))
	       (fac (/ (- max min) dur)))
	  (lambda (time)
	    (+ min (* time fac))))
	;; exponential case:
	(let* ((dur (calc-dur min max end-time))
	       (b (/ end-time (- a 1))))
	  (lambda (time)
	    (* b (/ (log a) dur) (expt a (/ time dur))))))))

(defun time->speed-fn (min max end-time)
  (lambda (time)
    (+ min (* (/ time end-time) (- max min)))))

(export '(make-mt-stream new-permutation jbmf rt-wait rt-sprout rt-proc drunk-traverse r-interpl time->vstime-fn vstime->time-fn time->speed-fn vstime->speed-fn calc-dur chord-derive display play-midi play-svg) 'cm)
