;;; 
;;; sequencer.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2019 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(in-package :cl-poolplayer)

(defparameter *sequences* nil)

(defstruct seq-player
  (playing t :type boolean)
  (fn nil))

;;; (make-seq-player)

(defstruct p-song
  (name "")
  (playfn #'identity)
  (durfn (lambda () (r-exp 10 15)))
  (beforefn (lambda () 0))
  (afterfn (lambda () 0)))

;;; (make-p-song)

(defun play-song (song &rest args)
  (let ((delay (getf args :delay 0)))
    (remf args :delay)
    (at (+ (now) delay)
        #'apply
        (p-song-playfn song)
        (getf args :duration (funcall (p-song-durfn song)))
        args)))

(defun playpattern (idxstream rstream start dur dtime &rest args)
  "play a pattern of idxs (into *buffers*) given by idxstream with given
rhythm stream."
  (let ((end (+ start dur))
        (outfn (getf args :outfn #'global-out))
        (amp (getf args :amp 0))
        (ampfn (lambda (x) (n-lin (interp x 0 0 0.3 1 0.7 1 1 0) -20 0))))
    (labels ((recurse-fn (time)
               (if (< time end)
                   (let ((next (+ time dtime))
                         (x (/ (- time start) dur))
                         (nextidx (next idxstream)))
                     (if (next rstream)
                         (apply #'play-buffer-stretch-env-pan-out*
                                (aref *buffers* nextidx)
                                (append (get-pan-out x outfn)
                                        (list :amp (+ amp (funcall ampfn x))) args)))
                     (at next #'recurse-fn next)))))
      (remf args :outfn)
      (remf args :amp)
      (recurse-fn start))))

#|

(defun get-streams (num pool)
  (values (new cycle :of (n-elts num pool))
          (new cycle :of (next (new weighting :of '((t :max 2) (nil :max 2))) num))))

|#

(defun start-seq (fn)
"start a sequence by creating a seq-player and calling the (endless)
recursive function fn. The seq-player contains a 'playing slot which
is initially set to t and checked on every recursion. If it is set to
nil the reursion stops and fn returns. To enable access to the
seq-player, it is pushed to *sequences*."
  (let ((s-player (make-seq-player :fn fn)))
    (push s-player *sequences*)
    (format t "~&starting ~a" fn)
    (funcall fn s-player)
    s-player))

#|

(defparameter *test* (make-seq-player :fn #'main-seq-01))

(funcall (seq-player-fn *test*) *test*)

|#
(defun stop-seq (s-player)
  "stop the sequence referred to by fn by calling :stop on it and
removing it from *sequences*."
  (setf (seq-player-playing s-player) nil)
  (setf *sequences* (remove s-player *sequences*)))

(defun stop-all-seqs ()
  "stop all pending sequences."
  (mapc #'stop-seq *sequences*)
  *sequences*)

;;; (setf *sequences* nil)
;;; (stop-all-seqs)

(defun show-playing-seqs ()
  (mapcar (lambda (s-player) (seq-player-fn s-player))
          *sequences*))

#|

(defun playfn (preset-no &optional params)
  (lambda (dur) (apply #'preset-play
                  (make-eventplayer :preset-no preset-no :dur dur params))))
|#

(defmacro def-seq (name pool &key dtimefn)
  (let ((dtfn (gensym "dtimefn")))
    `(defun ,name (s-player)
       (let (obj
             (,dtfn (or ,dtimefn (fnzero))))
         (labels ((recurse-fn (time stage)
                    (if (seq-player-playing s-player)
                        (progn
;;                          (if *show-song* (format t "~&obj: ~a stage: ~a" (if obj (song-name obj) nil) stage))
                          (case stage
                            (0 (let ((next-obj (next ,pool)))
                                 (setf obj (symbol-value next-obj))
                                 (let* ((dur (funcall (p-song-beforefn obj)))
                                        (next (+ time dur)))
                                   (if *show-song*
                                       (format t "~&~a in ~a Sekunden" (p-song-name obj) dur))
                                   (at next #'recurse-fn next (1+ stage)))))
                            (1 (let* ((dur (funcall (p-song-durfn obj)))
                                      (next (+ time dur)))
                                   (if *show-song*
                                       (format t "~&Spiele ~a für ~a Sekunden" (p-song-name obj) dur))
                                   (funcall (p-song-playfn obj) dur)
                                 (at next #'recurse-fn next (1+ stage))))
                            (2 (let* ((dur (funcall (p-song-afterfn obj)))
                                      (next (+ time dur)))
                                 (if *show-song* (format t "~&Pause für ~a Sekunden" dur))
                                 (at next #'recurse-fn next (1+ stage))))
                            (3 (let* ((dur (funcall ,dtfn))
                                      (next (+ time dur)))
                                 (if *show-song* (format t "~&Generalpause für ~a Sekunden" dur))
                                 (at next #'recurse-fn next 0)))))
                        )
                    ;; (print "done.")
                    ))
           (format t "~%")
           (recurse-fn (now) 0))))))
