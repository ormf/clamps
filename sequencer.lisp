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

(defstruct song
  (name "")
  (playfn #'identity)
  (durfn (lambda () (r-exp 10 15)))
  (beforefn (lambda () 0))
  (afterfn (lambda () 0)))

;;; (make-song)

(defun play-song (song)
  (funcall (song-playfn song)
           (funcall (song-durfn song))))

(defun playpattern (idxstream rstream start dur dtime &rest args)
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

(defun get-streams (num pool)
  (values (new cycle :of (n-elts num pool))
          (new cycle :of (next (new weighting :of '((t :max 2) (nil :max 2))) num))))


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

(defun playfn (preset params)
  (lambda (dur) (apply #'preset-play
                  (make-instance 'eventplayer) preset dur params)))

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
                                 (let* ((dur (funcall (song-beforefn obj)))
                                        (next (+ time dur)))
                                   (if *show-song*
                                       (format t "~&~a in ~a Sekunden" (song-name obj) dur))
                                   (at next #'recurse-fn next (1+ stage)))))
                            (1 (let* ((dur (funcall (song-durfn obj)))
                                      (next (+ time dur)))
                                   (if *show-song*
                                       (format t "~&Spiele ~a für ~a Sekunden" (song-name obj) dur))
                                   (funcall (song-playfn obj) dur)
                                 (at next #'recurse-fn next (1+ stage))))
                            (2 (let* ((dur (funcall (song-afterfn obj)))
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

;;;
;;;
;;;                   Song Definitions
;;;
;;;   ab 1.8.-14.8. nicht, sonst ok bis 23.8.

#|
(defun def-sequences ()
  (defparameter holz-01
    (make-song
     :name "holz-01"
     :playfn
     (playfn 0
             (list
              :g1 *pool0*
              :outfn #'global-out
              :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
              :amp -3
              :dtimefn (get-dtime-fn
                        0.05 0.1 2 7
                        :distribution '(1 1 1 1 1 1 1 1 1 2 2 2 3 4 5))))
     :beforefn (lambda () (r-exp 2 5))
     :durfn (lambda () (r-exp 10 25))
     :afterfn (lambda () (r-exp 2 5))))

  (defparameter teppichtrampeln                                                 
    (make-song
     :name "teppichtrampeln"
     :afterfn (lambda () (r-exp 2 5))                                                
     :playfn                                                                    
     (playfn 15                                                                 
             (list                                                              
             :lsamplefn (lambda (x) x (r-elt *pool12*)) 
             :outfn (fig12-out (random 9) *circle-cw*)
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -50))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0))))))))
 
;;;  (setf *net-debug* t)
    (defparameter teppich-dicht
      (make-song
       :name "teppich-dicht"
       :afterfn (lambda () (r-exp 2 5))
       :durfn (lambda () (r-exp 22 40))
       :playfn (lambda (dur)
                 (let ((start (now))
                       (mindur 1.0))
                   (labels ((durfn () (r-exp 4 9))
                            (recurse-fn (time dur end)
                              (funcall (song-playfn teppichtrampeln) dur)
                              (let* ((next (+ time (r-exp 0.5 (- (max 4 dur) 2))))
                                     (nextdur (marginclip (durfn) (- end next) mindur)))
                                (if (and (< next end)
                                         (> nextdur mindur))
                                    (at next #'recurse-fn next nextdur end)))))
                     (recurse-fn start (durfn) (+ start dur)))))))

  (defparameter teppich-dicht
    (make-song
     :name "teppich-dicht"
     :afterfn (lambda () (r-exp 2 5))
     :durfn (lambda () (r-exp 10 25))
     :playfn (lambda (dur)
               (let ((start (now))
                     (mindur 1.0))
                 (labels ((durfn () (r-exp 2 5))
                          (recurse-fn (time dur end)
                            (funcall (song-playfn teppichtrampeln) dur)
                            (let* ((next (+ time (r-exp 1.99 (- (max 4 dur) 2))))
                                   (nextdur (marginclip (durfn) (- end next) mindur)))
                              (if (and (< next end)
                                       (> nextdur mindur))
                                  (at next #'recurse-fn next nextdur end)))))
                   (recurse-fn start (durfn) (+ start dur)))))))

  (defparameter donnerblech-01
    (make-song
     :name "donnerblech-01"
     :playfn
     (playfn 15
             (list
              :lsamplefn (lambda (x) x (r-elt *pool5*)) 
              :outfn (fig12-out 0 (pick *rline* *line*))
              :transpfn (lambda (x) x (r-lin (+ (random 5.0) -50) 0))
              :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) -16 -32))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.02 2))))))

  (defparameter drone-01
    (make-song
     :name "drone-01"
     :durfn (lambda () (r-exp 10 18))
     :playfn
     (playfn 15

             
             (list
              :lsamplefn (lambda (x) x (r-elt *pool9*)) 
              :outfn (fig-out (random 9) (pick *line* *rline* *circle-cw* *circle-ccw*))
              :transpfn (lambda (x) x 0)
              :endfn (lambda (x) (n-exp x 0.3 1))
              :stretchfn (lambda (x) x 10)
              :wwidthfn (lambda (x) (n-exp (interp x 0 0 0.1 0 1 0) 10 1))
              :ampfn (lambda (x) (n-lin (interp x 0 0.5 0.1 0 1 1) (- -12 (random 12) ) -22))
              :attackfn (lambda (x) (n-lin x 0.1 0))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1)
                                     (+ 0.01 (random 0.1))
                                     (+ 0.1 (random 2.0))))))))

  (defparameter drone-hoch
    (make-song
     :name "drone-hoch"
     :durfn (lambda () (r-exp 10 18))
     :playfn
     (playfn 15
             (list
              :lsamplefn (lambda (x) x (r-elt *pool9*)) 
              :outfn (fig-out (random 13) (pick *circle-cw* *circle-ccw*))
              :transpfn (lambda (x) x 0)
              :endfn (lambda (x) (n-exp x 0.3 1))
              :stretchfn (lambda (x) x 10)
              :wwidthfn (lambda (x) x (/ 10 16))
              :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -12 (random 12) ) -22))
              :attackfn (lambda (x) (n-lin x 0.1 0))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.03 (random 0.1)) (+ 0.1 (random 2))))))))

  (defparameter metall-01
    (make-song
     :name "metall-01"
     :durfn (lambda () (r-exp 3 10))
     :playfn
     (playfn 0
             (list
              :g1 *pool5*
              :outfn #'global-out
              :transpfn (lambda (x) x (r-lin (+ (random 5.0) 50) 0))
              :amp -9
              :dtimefn (lambda (x) (n-exp (interp x 0 0.5 0.3 1 0.5 1 1 0) 1 0.1))))))

  (defparameter metall-02
    (make-song
     :name "metall-02"
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 8 20))
     :playfn
     (playfn 15
             (list
              :lsamplefn (lambda (x) x (r-elt *pool5*)) 
              :outfn #'global-out
              :transpfn (lambda (x) x (r-lin (+ (random 5.0) 50) 0))
              :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) -6 -22))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.02 2))))))

  (defparameter blech-01
    (make-song
     :name "blech-01"
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 40 48))
     :playfn
     (playfn 15
             (list
              :lsamplefn (lambda (x) x (r-elt *pool14*)) 
              :outfn #'global-out
              :transpfn (lambda (x) x (pick 0 4 7 12 -12))
              :stretchfn (lambda (x) x (r-exp 1 1))
              :wwidthfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) 123 1))
              :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0))))))))

  (defparameter chimes-01
    (make-song
     :name "chimes-01"
;;     :beforefn (lambda () (r-exp 3 10))
;;     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 8 14))
     :playfn
     (playfn 1
      (list
       :g1 *pool1*
       :outfn (fig12-out 0 (pick *line* *rline*))
       :g3 (lambda (x) (interp x 0 20 1 -50))
       :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30))))))

  (defparameter schleifklang-01
    (make-song
     :name "schleifklang-01"
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 10 30))
     :playfn
     (playfn 0
      (list
       :g1 *pool20*
       :outfn #'global-out
       :transpfn (lambda (x) x (r-lin (- -23  (random 20.0)) -20))
       :amp (- -23 (random 12))
       :stretchfn (lambda (x) x (r-exp 1 2))
       :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7))))))

  (defparameter mallet-01
    (make-song
     :name "mallet-01"
     :durfn (lambda () (r-exp 1 3))
     :playfn
     (playfn 0
      (list
       :g1 *pool1*
       :outfn #'global-out
       :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
       :amp -3
       :dtimefn (get-dtime-fn 0.05 0.1 0.1 0.3 :distribution '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5))))))

  (defparameter mallet-02
    (make-song
     :name "mallet-02"
     :durfn (lambda () (r-exp 3 5))
     :playfn
     (playfn 0
      (list
       :g1 *pool1*
       :outfn #'global-out
       :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
       :amp -3
       :dtimefn (get-dtime-fn 0.05 0.1 1 3 :distribution '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5))))))

  (defparameter e-guitar-01
    (make-song
     :name "e-guitar-01"
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 25 45))
     :playfn
     (playfn 0
      (list
       :g1 *pool21*
       :outfn #'global-out
       :transpfn (lambda (x) x (r-lin 0 0))
       :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
       :stretchfn (lambda (x) x (r-exp 1 2))
       :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7))))))

  (defparameter holzwirbel-01
    (make-song
     :name "holzwirbel-01"
     :durfn (lambda () (r-exp 6 11))
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :playfn
     (playfn 80
             (list
              :g1 *pool0*
              :outfn (fig12-out 0 (pick *line* *rline*))
              :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.03 0.04))
              :g3 (lambda (x) (interp x 0 10 1 10))
              :g2 (lambda (x) (interp x 0 -20 0.3 -10 1 -30))))))
  
  (defparameter holzregen-01
    (make-song
     :name "holzregen-01"
     :durfn (lambda () (r-exp 90 200))
     :afterfn (lambda () (r-exp 13 25))
     :playfn
     (playfn 80
             (list
             :g1 *pool0*
             :outfn #'global-out
             :dtimefn (lambda (x) (r-exp 0.02 (interp x 0 0.03 0.5 0.2 0.6 0.03 0.8 0.03 1 0.2)))
             :ampfn (lambda (x) (interp x 0 -10 0.1 0 0.7 0 1 -10))
             :g3 (lambda (x) (r-lin 10 (interp x 0 10 1 -10)))
             :g2 (lambda (x) (interp x 0 -20 0.3 -26 0.5 -26 0.6 -20 1 -60))))))

  (defparameter holzregen-02
    (make-song
     :name "holzregen-02"
     :durfn (lambda () (r-exp 60 90))
     :afterfn (lambda () (r-exp 13 25))
     :playfn
     (lambda (dur)
       (let ((start (now))
             (mindur 1.0))
         (labels ((durfn () (r-exp 20 35))
                  (recurse-fn (time dur end)
                    (funcall (song-playfn holzregen-01) dur)
                    (let* ((next (+ time (r-exp 1.99 2)))
                           (nextdur (marginclip (durfn) (- end next) mindur)))
                      (if (and (< next end)
                               (> nextdur mindur))
                          (at next #'recurse-fn next nextdur end)))))
           (recurse-fn start (durfn) (+ start dur)))))))

  (defparameter holzparadox-01
    (make-song
     :name "holzparadox-01"
     :durfn (lambda () (r-exp 9 12))
     :afterfn (lambda () (r-exp 3 5))
     :beforefn (lambda () (r-exp 3 5))
     :playfn
     (lambda (dur) 
       (let* ((params (list
                       :g1 *pool0*
                       :outfn (fig12-out 0 *line* :base 1/1000)
                       :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6)))))
              (repeat (+ 9 (random 4)))
              (players (loop for i below repeat collect (make-instance 'eventplayer))))
         (labels ((recurse-fn (time idx p)
                    (if (< idx repeat)
                        (let* ((duration (n-exp (/ idx (1- repeat)) (/ dur 5) dur))
                               (next (+ time (* 0.2 duration))))
                          (apply #'preset-play (first p) 0 duration
                                 :transpfn (lambda (x) (n-lin x (- -13 (* 1.7 idx)) 30))
                                 :amp (+ (random 6) -6 (* idx -1.5))
                                 params)
                          (at next
                              #'recurse-fn next (incf idx) (cdr p))))))
           (recurse-fn (now) 0 players))))))

  (defparameter afro-01
    (make-song
     :name "afro-01"
     :durfn (lambda () (r-exp 45 90))
     :afterfn (lambda () 0)
     :beforefn (lambda () 0)
     :playfn
     (lambda (dur)
       (let* ((numstream (new cycle :of '(7 15 11 13)))
              (now (now))
              (end (+ now dur))
              (args (list :amp -20 :transp (- 10 (random 20)))))
         (labels ((recurse-fn (time)
                    (if (< time end)
                        (let* ((pattern-dtime (+ 3.5 (* 0.1 (random 15))))
                               (next (+ time pattern-dtime))
                               (pattern-dur (+ 25 (random 10))))
                          (multiple-value-bind (pstream rstream) (get-streams (next numstream) *pool31-idxs*)
                            (apply #'playpattern pstream rstream time pattern-dur 0.1
                                   (append (list :outfn (fig5-out (random 5) *scircle-cw* :scale 2)) args)))
                          (if (< next end) (at next #'recurse-fn next))))))
           (recurse-fn now))))))
  
  (defparameter aitken-minimal-01
    (make-song
     :name "aitken-minimal-01"
     :durfn (lambda () (r-exp 45 90))
     :afterfn (lambda () 0)
     :beforefn (lambda () 0)
     :playfn
     (lambda (dur)
       (let* (
              (numstream (new cycle :of '(7 15 11 13)))
              (now (now))
              (end (+ now dur))
              (args (list :amp -20 :transp (- 10 (random 20)))))
         (labels ((recurse-fn (time)
                    (if (< time end)
                        (let* ((pattern-dtime (+ 3.5 (* 0.1 (random 15))))
                               (next (+ time pattern-dtime))
                               (pattern-dur (+ 25 (random 10))))
                          (multiple-value-bind (pstream rstream) (get-streams (next numstream) *pool2-idxs*)
                            (apply #'playpattern pstream rstream time pattern-dur 0.1
                                   (append (list :outfn (fig5-out (random 5) *scircle-cw* :scale 2)) args)))
                          (if (< next end) (at next #'recurse-fn next))))))
           (recurse-fn now))))))

  (def-seq main-seq-01
      (new weighting
        :of '((holz-01 :weight 18)
              (teppich-dicht :max 1)
              (teppichtrampeln :weight 2)
              (blech-01 :max 1)
              (mallet-02 :max 1)
              (holzwirbel-01)
              (holzparadox-01 :max 1)
              (holzregen-01 :max 1)
              (holzregen-02 :max 1)
              (metall-02 :max 1)
              (e-guitar-01 :max 1)
;;              (afro-01 :max 1)
;;              (aitken-minimal-01 :max 1)
              (chimes-01 :max 1)
              (donnerblech-01 :max 1))))

  (def-seq main-seq-02
      (new weighting
        :of '((drone-01 :weight 4)
;;              (afro-01 :max 1)
              (schleifklang-01 :max 1)
              (drone-hoch :max 1)))
    :dtimefn (get-dtime-fn-no-x
              5 20 300 600
              :distribution '((1 :weight 4) (3 :max 1))))

  (def-seq main-seq-03
      (new weighting
        :of '((aitken-minimal-01 :max 1)
              (afro-01 :max 1)))
    :dtimefn (get-dtime-fn-no-x
              5 20 300 900
              :distribution '((1 :weight 4) (3 :max 1)))))

|#


#|
(progn
  (defparameter holzregen-02
    (make-song
     :durfn (lambda () (r-exp 60 90))
     :afterfn (lambda () (r-exp 13 25))
     :playfn
     (lambda (dur)
       (let ((start (now))
             (mindur 1.0))
         (labels ((durfn () (r-exp 20 35))
                  (recurse-fn (time dur end)
                    (funcall (song-playfn holzregen-01) dur)
                    (let* ((next (+ time (r-exp 1.99 2)))
                           (nextdur (marginclip (durfn) (- end next) mindur)))
                      (if (and (< next end)
                               (> nextdur mindur))
                          (at next #'recurse-fn next nextdur end)))))
           (recurse-fn start (durfn) (+ start dur)))))))
  (play-song holzregen-02))




(incudine::live-nodes)

(loop for x from 0 to 100 do (apply #'play-buffer-stretch-env-pan-out* (aref *buffers* 0) '( :amp -56 :end 14)))


                                        ;
|#

;;; (progn (play-song teppich-dicht))
;;; (play-song holzparadox-01)

#|
(defun holzparadox-dicht
    (lambda (dur) ()))
|#

(defun n-elts (n seq)
  (next (new heap :of (coerce seq 'list)) n))

;;; (def-sequences)
;;; (stop-all-seqs)
;;; (start-seq #'main-seq-01)
;;; (start-seq #'main-seq-02)
;;; (show-playing-seqs)


;;; (show-playing-seqs)

;;;;



#|

(play-song holz-01)
(play-song teppich-dicht)
(play-song teppichtrampeln)
(play-song blech-01)
(play-song mallet-01)
(play-song mallet-02)
(play-song holzwirbel-01)
(play-song holzparadox-01)
(play-song holzregen-01)
(play-song metall-02)
(play-song e-guitar-01)
(play-song chimes-01)
(play-song donnerblech-01)
(play-song afro-01)
(play-song aitken-minimal-01)


(play-song holz-01)
(play-song mallet-01)
(play-song mallet-02)
(play-song holzwirbel-01)
(play-song holzparadox-01)
(play-song holzregen-01)
(play-song metall-02)
(play-song e-guitar-01)
(play-song chimes-01)
(play-song blech-01)
(play-song donnerblech-01)
(play-song teppichtrampeln)
(play-song teppich-dicht)


(play-song drone-01)


(play-song drone-hoch)


(stop-all-seqs)

(def-sequences)



(def-seq main-seq-02
      (new weighting
        :of '((drone-01 :weight 4)
              (drone-hoch :max 1))))

(get-dtime-fn-no-x 5 20 300 600 :distribution '((1 :weight 4) (3 :max 1)))



(def-seq main-seq-02
      (new weighting
        :of '((drone-01 :weight 4)
              (drone-hoch :max 1)))
  :dtimefn (get-dtime-fn 5 20 300 600 :distribution '((1 :weight 4) (3 :max 1))))

(let ((fn (get-dtime-fn 5 20 300 600 :distribution '((1 :weight 4) (3 :max 1)))))
  (loop repeat 10 collect (funcall fn 0)))


(def-seq main-seq-01
      (new weighting
        :of '((holz-01 :weight 4)
              (teppich-dicht :max 1)
              teppichtrampeln
              (donnerblech-01 :max 1))))
(make-song 
 :playfn (song-playfn teppich-dicht)
 :beforefn (lambda () 0)
 :afterfn (lambda () (0))
 :durfn (lambda () (r-exp 12 25))
 )

  (defparameter *seq-pool-01*
  (new weighting
    :of '((holz-01 :weight 4)
          (teppich-dicht :max 1)
          teppichtrampeln)))

(defun main-seq-01 (s-player)
  (let (obj)
    (labels ((recurse-fn (time stage)
               (if (seq-player-playing s-player)
                   (case stage
                     (0 (let ((next-obj (next *seq-pool-01*)))
                          (setf obj (symbol-value next-obj))
                          (let* ((dur (funcall (song-beforefn obj)))
                                 (next (+ time dur)))
                            (if *show-song* (format t "~&~a in ~a Sekunden" next-obj dur))
                            (at next #'recurse-fn next (mod (incf stage) 3)))))
                     (1 (let* ((dur (funcall (song-durfn obj)))
                               (next (+ time dur)))
                          (funcall (song-playfn obj) dur)
                          (at next #'recurse-fn next (mod (incf stage) 3))))
                     (2 (let* ((next (+ time (funcall (song-afterfn obj)))))
                          (at next #'recurse-fn next (mod (incf stage) 3))))))
               ;; (print "done.")
               ))
      (recurse-fn (now) 0))))



                                      ; ;

(stop-all-seqs)

(defun main-seq-02 (s-player)
  (labels ((durfn () (+ 2 (random 5.0)))
           (recurse-fn (time)
             (if (seq-player-playing s-player)
                 (progn
                   (let* ((dur (durfn))
                          (next (+ time dur)))
                     (funcall (song-playfn teppichtrampeln) dur)  
                     (at next #'recurse-fn next)))
                 (print "done."))))
    (recurse-fn (now))))

(stop-all-sequences)                                      ;
(def-sequences)
(funcall (song-durfn teppichtrampeln))
(funcall (song-playfn teppichtrampeln) 7)
(funcall (song-playfn teppich-dicht) 17)
(funcall (song-playfn holz-01) 17)

(song-playfn teppichtrampeln)                                      ;
(make-song :durfn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :durfn (lambda () (r-exp 10 15)))
              :beforefn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :beforefn (lambda () 0))
              :afterfn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :afterfn (lambda () 0)))


(let ((plist (list :afterfn (lambda () (r-exp 2 5)) :lsamplefn
   (lambda (x) x (r-elt *pool12*)) :outfn
   (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
   :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
   :ampfn
   (lambda (x)
     (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
   :dtimefn
   (lambda (x)
     (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
            (+ 1.5 (random 2.0)))))))
  (rmprop
  plist                                     
  :afterfn 4))

(let ((plist '(:afterfn 3)))
  (rmprop plist
   :afterfn 2))

(progn
 (defparameter teppichtrampeln
   (make-song :durfn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :durfn (lambda () (r-exp 10 15)))
              :beforefn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :beforefn (lambda () 0))
              :afterfn
              (rmprop
               '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
                 (lambda (x) x (r-elt *pool12*)) :outfn
                 (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                 :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                 :ampfn
                 (lambda (x)
                   (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
                 :dtimefn
                 (lambda (x)
                   (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                          (+ 1.5 (random 2.0)))))
               :afterfn (lambda () 0))))
 (defun teppichtrampeln (#:|dur1747|) 
   (apply #'preset-play (make-instance 'eventplayer) 0 #:|dur1747|
          '(:afterfn (lambda () (r-exp 2 5)) :lsamplefn
            (lambda (x) x (r-elt *pool12*)) :outfn
            (fig12-out (random 9) (pick *circle-ccw* *circle-cw*)) :transpfn
            (lambda (x) x (r-lin (+ (random 5.0) 10) -20)) :ampfn
            (lambda (x)
              (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12)) -32))
            :dtimefn
            (lambda (x)
              (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02))
                     (+ 1.5 (random 2.0))))))))
|#


;;; (show-playing-seqs)

#|

(defun teppichtrampeln (dur)
  (let ((params
          (list :lsamplefn (lambda (x) x (r-elt *pool12*)) 
                :outfn (fig12-out (random 9) (pick *circle-ccw* *circle-cw*))
                :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
                :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1)
                                       (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))))
    (apply #'preset-play
         (make-instance 'eventplayer) 15 dur params)))

;;; (teppichtrampeln 3)

(defun holz-01 (dur)
  (let ((params
          (list
           :g1 *pool0*
           :outfn #'global-out
           :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
           :amp -3
           :dtimefn (get-dtime-fn 0.05 0.1 2 7 :distribution '(1 1 1 1 2 2 2 3 4 5)))))  
    (apply #'preset-play
           (make-instance 'eventplayer) 0 dur params)))



;;; (marginclip 5.5 7 1) -> 5.5
;;; (marginclip 6.5 7 1) -> 7
;;; (marginclip 8 7 1) -> 7

(defun teppich-dicht (dur)
  (let ((start (now))
        (mindur 1.0))
    (labels ((durfn () (r-exp 2 5))
             (recurse-fn (time dur end)
               (teppichtrampeln dur)
               (let* ((next (+ time (r-exp 2 (- dur 2))))
                      (nextdur (marginclip (durfn) (- end next) mindur)))
                 (if (and (< next end)
                          (> nextdur mindur))
                     (at next #'recurse-fn next nextdur end)
                     (print "done.")))))
      (recurse-fn start (durfn) (+ start dur)))))

;;; (holz-01 100)
;;; (teppich-dicht 20)



(defparameter)

(let ((plist '(:out 3 :amp 2)))
  (list (rmprop plist :out 5)
        plist))



(defparameter holz-01
  (make-song :playfn #'holz-01
             :beforefn (lambda () (r-exp 2 5))
             :afterfn (lambda () (r-exp 2 5))))

(defparameter teppichtrampeln
  (make-song :fn #'teppichtrampeln
             :afterfn (lambda () (r-exp 2 5))))

(defparameter teppich-dicht
  (make-song :fn #'teppich-dicht
             :afterfn (lambda () (r-exp 2 5))))

|#



;;; (start-seq #'main-seq-01)

;;; (setf *show-song* t)

;;; (show-playing-seqs)

;;; (stop-all-seqs)
;;; (stop-seq (first *sequences*))

#|
;;; deprecated approach:

(defun block-play (fn dur)
  "play fn for dur and return after dur"
  (funcall fn dur)
  (sleep dur))

(loop
  repeat 10
  for fn = (pick #'holz-01 #'teppichtrampeln)
  for dur = (+ 10 (random 10))
  do (block-play fn dur))

(defun play-song (time repeat)
  (let ((dur (+ 5 (random 10))))
    (format t "playsong: ~a, " repeat)
    (teppichtrampeln dur)
    (if (> repeat 1)
        (at (+ time dur) #'play-song (+ time dur) (decf repeat)))))

;;; Buster Keaton Gags: https://www.youtube.com/watch?v=UWEjxkkB8Xs

|#



#|
(start-seq #'main-seq-01)

(stop-seq (first *sequences*))

(stop-seq (first *sequences*))

(setf *sequences* nil)

(stop-all-seqs)
(stop-seq (first *sequences*))


;;; (setf *sequences* nil)

(teppichtrampeln 0.8)
(untrace)


  (loop
    repeat 10
    for 
    do (progn
         (teppichtrampeln dur)
         (sleep )))
|#


