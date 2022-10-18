;;; 
;;; scratch.lisp
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


(aref *presets* 0)



(digest-bo-preset
 97
 (:p1 0
  :p2 0
  :p3 0
  :p4 0
  :dtimefn (funcall (or (getf args :dtimefn) (lambda (x) (n-exp x 0.05 0.2))) x)
  :lsamplefn (r-elt (getf args :g1))
  :ampfn (funcall
          (or (getf args :ampfn)
              (lambda (x) x (+ (or (getf args :amp 0)) (random 12) -6)))
          x)
  :transpfn (funcall
             (getf args :transpfn
                   (lambda (x) (r-lin (n-lin x -30 40) (n-lin x -30 80))))
             x)
  :startfn 0
  :endfn 0
  :stretchfn (r-exp 1 1)
  :wwidthfn 123
  :attackfn 0
  :releasefn 0.01
  :outfn (funcall (getf args :outfn #'stereo-out) x)))

;;;play-buffer-stretch-env-out

(at (+ (now) 1) #'play-buffer-stretch* :buffer of-incudine-dsps::*buf* :end 0.2)

(at (+ (now) 1) #'play-buffer-stretch* :buffer (aref *buffers* 0) :end 3 :amp -12)


(digest-audio-args-preset
 '(:p1 1
   :p2 (- p1 1)
   :p3 0
   :p4 0
   :pitchfn (+ p2 (n-exp y 0.4 1.08))
   :ampfn (progn (* (/ v 20) (sign) (n-exp y 3 1.5)))
   :durfn 0.5
   :suswidthfn 0
   :suspanfn (random 1.0)
   :decay-startfn 5.0e-4
   :decay-endfn 0.002
   :lfo-freqfn (r-exp 50 80)
   :x-posfn x
   :y-posfn y
   :wetfn 1
   :filt-freqfn 20000)
 (aref *audio-presets* 0))

(defparameter *pool0-idxs* (mapcar #'buf-idx (coerce *pool0* 'list)))
(defparameter *pool1-idxs* (mapcar #'buf-idx (coerce *pool1* 'list)))
(defparameter *pool2-idxs* (mapcar #'buf-idx (coerce *pool2* 'list)))
(defparameter *pool31-idxs* (mapcar #'buf-idx (coerce *pool31* 'list)))



    (play-buffer-stretch-env-pan-out (aref *buffers* (next samplestream)))

(untrace)

(defun playpattern (idxstream rstream start dur dtime &rest args)
  (let ((end (+ start dur))
        (outfn (getf args :outfn #'global-out)))
    (labels ((recurse-fn (time)
               (if (< time end)
                   (let ((next (+ time dtime))
                         (x (/ (- time start) dur)))
                     (if (next rstream)
                         (apply #'play-buffer-stretch-env-pan-out*
                                (aref *buffers* (next idxstream))
                                (append (get-pan-out x outfn) args)))
                     (at next #'recurse-fn next)))))
      (remf args :outfn)
      (recurse-fn start))))

(get-pan-out 0.1 (fig12-out 0 *circle-cw*))

(defun playpattern (idxstream rstream start dur dtime &rest args)
  (let ((end (+ start dur))
        )
    (labels ((recurse-fn (time)
               (if (< time end)
                   (let ((next (+ time dtime))
                         (x (/ (- time start) dur))
                         (nextidx (next idxstream)))
                     (if (next rstream)
                         (apply #'play-buffer-stretch-env-pan-out*
                                (aref *buffers* nextidx)
                                args))
                     (at next #'recurse-fn next)))))
      (recurse-fn start))))

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

(fig12-out 0 *circle-cw*)

(let ((dur 30)
      (stream1 (new cycle :of (n-elts 7 *pool31-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t nil)))
      (stream2 (new cycle :of (n-elts 5 *pool31-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool31-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool31-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -20 :transp (- 10 (random 20)))))
  (at now
      (lambda () (apply #'playpattern stream1 rstream1 now dur 0.1
                   (append (list :outfn (fig5-out 0 *scircle-cw* :scale 2)) args))))
  (at (+ now 1)
       (lambda () (apply #'playpattern stream2 rstream2 now dur 0.1
                    (append (list :outfn (fig5-out 1 *scircle-cw* :scale 2)) args))))
  (at (+ now 2)
      (lambda () (apply #'playpattern stream3 rstream3 now dur 0.1
                             (append (list :outfn (fig5-out 2 *scircle-cw* :scale 2)) args))))
  (at (+ now 3)
      (lambda () (apply #'playpattern stream4 rstream4 now dur 0.1
                             (append (list :outfn (fig5-out 3 *scircle-cw* :scale 2)) args))))
  )





(defun get-streams (num pool)
  (values (new cycle :of (n-elts num pool))
          (new cycle :of (next (new weighting :of '((t :max 2) (nil :max 2))) num))))

(defparameter *test* t)

(let* ((dur 60)
       (numstream (new cycle :of '(7 15 11 13)))
       (now (now))
       (end (+ now dur))
       (args (list :amp -20 :transp (- 10 (random 20)))))
  (labels ((recurse-fn (time)
             (if (and (< time end) *test*)
                 (let* ((pattern-dtime (+ 3.5 (* 0.1 (random 15))))
                        (next (+ time pattern-dtime))
                        (pattern-dur (+ 25 (random 10))))
                   (multiple-value-bind (pstream rstream) (get-streams (next numstream) *pool2-idxs*)
                     (apply #'playpattern pstream rstream time pattern-dur 0.1
                            (append (list :outfn (fig5-out (random 5) *scircle-cw* :scale 2)) args)))
                   (if (< next end) (at next #'recurse-fn next))))))
    (recurse-fn now)))


(let ((dur 30)
      (stream1 (new cycle :of (n-elts 7 *pool31-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t nil)))
      (stream2 (new cycle :of (n-elts 5 *pool31-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool31-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool31-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -20 :transp (- 10 (random 20)))))
  (at now
      (lambda () (apply #'playpattern stream1 rstream1 now dur 0.1
                   (append (list :outfn (fig5-out 0 *scircle-cw* :scale 2)) args))))
  (at (+ now 1)
       (lambda () (apply #'playpattern stream2 rstream2 now dur 0.1
                    (append (list :outfn (fig5-out 1 *scircle-cw* :scale 2)) args))))
  (at (+ now 2)
      (lambda () (apply #'playpattern stream3 rstream3 now dur 0.1
                             (append (list :outfn (fig5-out 2 *scircle-cw* :scale 2)) args))))
  (at (+ now 3)
      (lambda () (apply #'playpattern stream4 rstream4 now dur 0.1
                             (append (list :outfn (fig5-out 3 *scircle-cw* :scale 2)) args))))
  )

(let ((dur 10)
      (stream1 (new cycle :of (n-elts 7 *pool31-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t nil)))
      (stream2 (new cycle :of (n-elts 5 *pool31-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool31-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool31-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -20 :transp (- 10 (random 20)))))
  (apply #'playpattern stream1 rstream1 now dur 0.1
         (append (list :outfn (fig5-out 0 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream2 rstream2 now dur 0.1
         (append (list :outfn (fig5-out 1 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream3 rstream3 now dur 0.1
         (append (list :outfn (fig5-out 2 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream4 rstream4 now dur 0.1
         (append (list :outfn (fig5-out 3 *scircle-cw* :scale 2)) args))
  )

(let ((dur 10)
      (stream1 (new cycle :of (n-elts 7 *pool31-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t nil)))
      (stream2 (new cycle :of (n-elts 5 *pool31-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool31-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool31-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -20 :transp (- 10 (random 20)))))
  (apply #'playpattern stream1 rstream1 now dur 0.1
         (append (list :outfn (fig5-out 0 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream2 rstream2 now dur 0.1
         (append (list :outfn (fig5-out 1 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream3 rstream3 now dur 0.1
         (append (list :outfn (fig5-out 2 *scircle-cw* :scale 2)) args))
  (apply #'playpattern stream4 rstream4 now dur 0.1
         (append (list :outfn (fig5-out 3 *scircle-cw* :scale 2)) args))
  )

(let ((dur 10)
      (stream1 (new cycle :of (n-elts 7 *pool2-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t nil)))
      (stream2 (new cycle :of (n-elts 5 *pool2-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool2-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool2-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -26 :transp (- 10 (random 20)))))
  (apply #'playpattern stream1 rstream1 now dur 0.1
         (append (list :out1 7 :pan 0) args))
  (apply #'playpattern stream2 rstream2 now dur 0.1
         (append (list :out1 2 :pan 0) args))
  (apply #'playpattern stream3 rstream3 now dur 0.1
         (append (list :out1 8 :pan 0) args))
  (apply #'playpattern stream4 rstream4 now dur 0.1
         (append (list :out1 3 :pan 0) args)))

(let ((dur 20)
      (stream1 (new cycle :of (n-elts 7 *pool31-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t t)))
      (stream2 (new cycle :of (n-elts 5 *pool31-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool31-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool31-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -6 :transp (- 10 (random 20)))))
  (apply #'playpattern stream1 rstream1 now dur 0.1
         (append (list :outfn (fig12-out 9 *circle-cw*)) args))
  (apply #'playpattern stream2 rstream2 now dur 0.1
         (append (list :outfn (fig12-out 8 *circle-cw*)) args))
  (apply #'playpattern stream3 rstream3 now dur 0.1
         (append (list :outfn (fig12-out 7 *circle-cw*)) args))
  (apply #'playpattern stream4 rstream4 now dur 0.1
         (append (list :outfn (fig12-out 6 *circle-cw*)) args)))


(let ((dur 10)
      (stream1 (new cycle :of (n-elts 7 *pool2-idxs*)))
      (rstream1 (new cycle :of '(t t nil t nil t t)))
      (stream2 (new cycle :of (n-elts 5 *pool2-idxs*)))
      (rstream2 (new cycle :of '(t nil t nil t)))
      (stream3 (new cycle :of (n-elts 11 *pool2-idxs*)))
      (rstream3 (new cycle :of '(t t nil t nil t t nil nil t nil)))
      (stream4 (new cycle :of (n-elts 13 *pool2-idxs*)))
      (rstream4 (new cycle :of '(t nil t t nil t nil t t nil nil t nil)))
      (now (now))
      (args (list :amp -16 :transp (- 10 (random 20)))))
  (apply #'playpattern stream1 rstream1 now dur 0.1 args)
  (apply #'playpattern stream2 rstream2 now dur 0.1 args)
  (apply #'playpattern stream3 rstream3 now dur 0.1 args)
  (apply #'playpattern stream4 rstream4 now dur 0.1 args))




(cm:rescale-envelope)




(progn
  (stop (aref *players* 0))
  (stop (aref *players* 1)))

(untrace)

(describe 'load-sounds)
(player-set 0 :dtime 0.1 :dtime-dev 10)
(player-set 1 :dtime 0.3 :dtime-dev 2)

(preset-play (aref *players* 0) 11 (+ 4 (random 10)))
(preset-play (aref *players* 1) 8 (+ 5 (random 4)))
(preset-play (aref *players* 2) (+ 10 (random 2)) (+ 7 (random 3)))
(preset-play (aref *players* 3) (+ 10 (random 2)) (+ 5 (random 4)))
(preset-play (aref *players* 4) 9 (+ 5 (random 4)))

(defun stereo-out (x)
  (declare (ignore x))
  (values 0 1 (random 1.0)))

(loop for x below 9 collect)

(goto-preset 1)
(goto-preset 99)

;;; mallet accelerando-ritardando
(preset-play (aref *players* 0) 13 (+ 50 (random 5.0))
             :lsamplefn (lambda () (r-elt *pool1*)))

(preset-play (aref *players* 0) 1 2
             :g1 *pool0*
             :outfn (fig-out (random 9) *circle-cw*)
             :g3 (lambda (x) (interp x 0 0 1 -0))
             :g2 (lambda (x) (interp x 0 -10 0.3 -0 1.1 -10)))


(preset-play (aref *players* 0) 1 2
             :g1 *pool0*
             :outfn (fig-out (random 9) *circle-cw*)
             :g3 (lambda (x) (interp x 0 0 1 -0))
             :g2 (lambda (x) (interp x 0 -10 0.3 -0 1.1 -10)))

(preset-play (aref *players* 0) 1 2
             :g1 *pool4*
             :outfn #'stereo-out
             :g3 (lambda (x) (interp x 0 0 1 -0))
             :g2 (lambda (x) (interp x 0 -10 0.3 -0 1.1 -10)))

;;; Obertonspektrum:
(preset-play (aref *players* 0) 1 3
             :g1 *pool0*
             :outfn (fig-out (random 13) *circle-ccw*)
             :g3 (lambda (x) (interp x 0 (* -1 (+ 20 (random 20))) 1 -30))
             :dtimefn (lambda (x) (r-exp (n-exp x 0.01 0.1) (n-exp x 0.03 5)))
             :g2 (lambda (x) (interp x 0 -30 0.2 -20 1.1 -10)))

(preset-play (aref *players* 0) 1 15
             :g1 *pool1*
             :outfn #'global-out
             :g3 (lambda (x) (interp x 0 (* -1 (+ 20 (random 20))) 1 -30))
             :dtimefn (lambda (x) (r-exp (n-exp x 0.01 0.1) (n-exp x 0.03 5)))
             :g2 (lambda (x) (interp x 0 -30 0.2 -20 1.1 -10)))

(preset-play (aref *players* 0) 1 20
             :g1 *pool0*
             :outfn #'global-out
             :g3 (lambda (x) (interp x 0 0 1 -0))
             :g2 (lambda (x) (interp x 0 -26 0.3 -10 1.1 -26)))

;;; Glocken hoch->tief
(preset-play (aref *players* 0) 1 3
             :g1 *pool1*
             :outfn (fig12-out 0 (pick *line* *rline*))
             :g3 (lambda (x) (interp x 0 20 1 -50))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30)))

(preset-play (aref *players* 0) 1 3
             :g1 *pool19*
             :outfn (fig12-out 0 (pick *line* *rline*))
             :g3 (lambda (x) (interp x 0 2 1 -5))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30)))

;;; test loudspeakers:

(preset-play (aref *players* 0) 15 24
             :lsamplefn (lambda (x) x (aref *buffers* 4))
             :outfn (fig12-out 0 *line*)
             :g3 (lambda (x) (interp x 0 2 1 -5))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30)))

;;;; (untrace)

;;; holz ritardando hoch->tief

(preset-play (make-instance 'eventplayer) 80 (+ 3 (random 4))
             :g1 *pool0*
             :outfn (fig13-out 0 (pick *line* *rline*))
             :g3 (lambda (x) (interp x 0 10 1 -20))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30)))

;;; Holz 
(preset-play (make-instance 'eventplayer) 80 (+ 3 (random 4))
             :g1 *pool0*
             :outfn (fig12-out 0 (pick *line* *rline*))
             :g3 (lambda (x) (interp x 0 10 1 0))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1.1 -30)))

(preset-play (make-instance 'eventplayer) 80 (+ 10 (random 4))
             :g1 *pool0*
             :outfn (fig12-out 0 (pick *line* *rline*))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.03 0.04))
             :g3 (lambda (x) (interp x 0 10 1 10))
             :g2 (lambda (x) (interp x 0 -30 0.3 -20 1 -50)))

;;; Holzregen-01

(preset-play (make-instance 'eventplayer) 80 (+ 20 (random 4))
             :g1 *pool0*
             :outfn #'global-out
             :dtimefn (lambda (x) (r-exp 0.02 (interp x 0 0.03 0.5 0.2 0.6 0.03)))
             :g3 (lambda (x) (r-lin 10 (interp x 0 10 1 -10)))
             :g2 (lambda (x) (interp x 0 -20 0.3 -20 1 -40)))

(preset-play (make-instance 'eventplayer) 80 (+ 20 (random 4))
             :g1 *pool0*
             :outfn #'global-out
             :dtimefn (lambda (x) (r-exp 0.02 (interp x 0 0.03 0.5 0.2 0.6 0.03)))
             :g3 (lambda (x) (r-lin 10 (interp x 0 10 1 -10)))
             :g2 (lambda (x) (interp x 0 -20 0.3 -20 1 -40)))


;;; einzelne Mallets, random transp

(preset-play (make-instance 'eventplayer) 0 70
             :g1 *pool2*
             :outfn (fig12-out 0 *line*)
             :amp -12
             :dtimefn (lambda (x) x (r-exp 0.4 7)))

;;; holz ritardando, transp tief->hoch

(preset-play (aref *players* 0) 0 10
             :g1 *pool0*
             :outfn (fig12-out 0 *line* :base 1/1000)
             :transpfn (lambda (x) (n-lin x (+ (random 5.0) -13) 30))
             :amp (+ (random 6) -6)
             :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6))))





(defun holzparadox-01 (dur)
  (let* ((params (list
                  :g1 *pool0*
                  :outfn (fig12-out 0 *line*)
                  :transpfn (lambda (x) (n-lin x (+ (random 5.0) -13) 30))
                  :amp (+ (random 6) -6)
                  :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6)))))
         (repeat 5)
         (players (loop for i below repeat collect (make-instance 'eventplayer))))
    (labels ((recurse-fn (time idx p)
               (if (< idx repeat)
                   (let ((next (+ time (n-exp (/ idx (1- repeat)) 1 0.4))))
                     (apply #'preset-play (first p) 0 (n-exp (/ idx (1- repeat)) 3 dur) params)
                     (at next
                         #'recurse-fn next (incf idx) (cdr p))))))
      (recurse-fn (now) 0 players))))

(defun holzparadox-01 (dur)
  (let* ((params (list
                  :g1 *pool0*
                  :outfn (fig12-out 0 *line* :base 1/1000)
                  :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6)))))
         (repeat (round (r-lin 9 12)))
         (players (loop for i below repeat collect (make-instance 'eventplayer))))
    (labels ((recurse-fn (time idx p)
               (if (< idx repeat)
                   (let* ((duration (n-exp (/ idx (1- repeat)) 2 dur))
                          (next (+ time (* 0.2 duration))))
                     (format t "~&idx: ~a, dur: ~a" idx duration)
                     (apply #'preset-play (first p) 0 duration
                            :transpfn (lambda (x) (n-lin x (- -13 (* 1.7 idx)) 30))
                            :amp (+ (random 6) -6 (* idx -1.5))
                            params)
                     (at next
                         #'recurse-fn next (incf idx) (cdr p))))))
      (recurse-fn (now) 0 players))))

(holzparadox-01 20)

(preset-play (aref *players* 0) 0 10
)


;;; Standardholz

(preset-play (aref *players* 0) 0 300
             :g1 *pool0*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
             :amp -3
             :dtimefn (get-dtime-fn 0.05 0.1 2 7 :distribution '(1 1 1 1 2 2 2 3 4 5)))


(preset-play (aref *players* 0) 0 30
             :g1 *pool1*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
             :amp -3
             :dtimefn (get-dtime-fn 0.05 0.1 8 17 :distribution
                                    '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5)))
;;; standard mit mallets

(preset-play (aref *players* 0) 0 30
             :g1 *pool30*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
             :amp (- -3 (random 12))
             :dtimefn (get-dtime-fn 0.05 0.1 12 17 :distribution
                                    '(1 1 2 2 2 3 4 5 6 7)))

;;; (stop (aref *players* 0))
;;; Sehr schön!

;;; schleifklang-01

(preset-play (aref *players* 0) 0 30
             :g1 *pool20*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (- -23  (random 20.0)) -20))
             :amp (- -13 (random 12))
             :stretchfn (lambda (x) x (r-exp 1 2))
             :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7)))

;;; E-Gitarren

(preset-play (aref *players* 0) 0 30
             :g1 *pool21*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin 0 0))
;;             :amp -3
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.9 0 1 1) (- -6 (random 12) ) -32))
             :stretchfn (lambda (x) x (r-exp 1 2))
             :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7)))

(preset-play (aref *players* 0) 0 3000
             :g1 *pool22*
             :outfn #'global-out
             :transpfn (lambda (x) x 0)
             :amp (- 0 )
             :stretchfn (lambda (x) x (r-exp 1 2))
             :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7)))

;;; holzmallet-01

(preset-play (make-instance 'eventplayer) 0 30
             :g1 *pool1*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
             :amp -3
             :dtimefn (get-dtime-fn 0.05 0.1 0.1 0.3 :distribution '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5)))

(progn
  (preset-play (make-instance 'eventplayer) 0 3
               :g1 *pool0*
               :outfn #'global-out
               :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
               :amp -3
               :dtimefn (get-dtime-fn 0.05 0.1 0.1 0.3 :distribution '(1 1 1 1 2 2 2 3 4 5)))
  )

(progn
  (preset-play (make-instance 'eventplayer) 0 3
               :g1 *pool0*
               :outfn #'global-out
               :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
               :amp -3
               :dtimefn (get-dtime-fn 0.05 0.1 0.1 0.3 :distribution '(1 1 1 1 2 2 2 3 4 5)))
  (preset-play (make-instance 'eventplayer) 0 10
               :g1 *pool1*
               :outfn #'global-out
               :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
               :amp -3
               :dtimefn (get-dtime-fn 0.05 0.1 0.1 0.3 :distribution '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5))))

;;; Holzpuls

(preset-play (make-instance 'eventplayer) 0 30
             :g1 *pool3*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -13) 10))
             :amp -3
             :dtimefn (get-dtime-fn 0.1 0.1 0.1 0.1 :distribution '(1 1 1 1 1 1 1 1 1 1 2 2 2 3 4 5)))

;;; Metall

(preset-play (make-instance 'eventplayer) 0 2
             :g1 *pool5*
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) 50) 0))
             :amp -3
             :dtimefn (lambda (x) (n-exp (interp x 0 0.5 0.3 1 0.5 1 1 0) 1 0.1)))

;;; Metall ritardando

(let ((dur 10))
  (preset-play (make-instance 'eventplayer) 15 dur
               :lsamplefn (lambda (x) x (r-elt *pool5*)) 
               :outfn #'global-out
               :transpfn (lambda (x) x (r-lin (+ (random 5.0) 50) 0))
               :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) -6 -22))
               :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.02 2))))

(let ((dur 50))
  (preset-play (make-instance 'eventplayer) 15 dur
               :lsamplefn (lambda (x) x (aref *pool1* 0)) 
               :outfn #'global-out
               :transpfn (lambda (x) (let ((time (* x dur)))
                                   (+ (mod (floor (/ time 0.4)) 2) 13)))
               :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) -6 -22))
               :dtimefn (lambda (x) x 0.1)))

;;; "Donnerblech" ritardando:

(preset-play (make-instance 'eventplayer) 15 15
             :lsamplefn (lambda (x) x (r-elt *pool5*)) 
             :outfn (fig12-out 0 (pick *rline* *line*))
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) -50) 0))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) -16 -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) 0.02 2)))

;;; teppichtrampeln

(preset-play (make-instance 'eventplayer) 15 (+ 10 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool12*)) 
             :outfn (fig12-out (random 9) *circle-cw*)
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -50))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))

(preset-play (make-instance 'eventplayer) 15 (+ 10 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool12*)) 
             :outfn (fig12-out (random 9) *circle-cw*)
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -50))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.2 (random 0.2)) (+ 1.5 (random 2.0)))))

;;; flageolettteppich selten

(preset-play (make-instance 'eventplayer) 15 (+ 40 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool13*)) 
             :outfn #'global-out
             :transpfn (lambda (x) x (r-lin (+ (random 5.0) 0) 10.0))
             :stretchfn (lambda (x) x (r-exp 0.2 0.1))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.2 (random 0.2)) (+ 1.5 (random 2.0)))))

;;; Blech 01

(preset-play (make-instance 'eventplayer) 15 10
             :lsamplefn (lambda (x) x (aref *pool14* 0)) 
             :outfn #'global-out
             :transpfn (lambda (x) (n-lin x 0 12))
             :stretchfn (lambda (x) x (r-exp 1 1))
             :wwidthfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) 123 1))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 0.1 0.5 0.3 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))

(preset-play (make-instance 'eventplayer) 15 (+ 40 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool14*)) 
             :outfn #'global-out
             :transpfn (lambda (x) x (pick 0 4 7 12 -12))
             :stretchfn (lambda (x) x (r-exp 1 1))
             :wwidthfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) 123 1))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))

;;; drone

(preset-play (make-instance 'eventplayer) 15 (+ 10 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool9*)) 
             :outfn (fig-out (random 9) (pick *line* *rline* *circle-cw* *circle-ccw*))
             :transpfn (lambda (x) x 0)
             :stretchfn (lambda (x) x 10)
             :wwidthfn (lambda (x) (n-exp (interp x 0 0 0.1 0 1 0) 10 1))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -12 (random 12) ) -22))
             :attackfn (lambda (x) (n-lin x 0.1 0))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1)
                                    (+ 0.03 (random 0.03))
                                    (+ 0.1 (random 2.0)))))

(preset-play (make-instance 'eventplayer) 15 25
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
                                    (+ 0.1 (random 2.0)))))

;;; Partialtonakkord leise

(preset-play (make-instance 'eventplayer) 16 0.2
             :lsamplefn (lambda (x) x (r-elt *pool9*)) 
             :outfn (fig-out (random 9) (pick *circle-cw* *circle-ccw*))
             :transpfn (lambda (x) x 0)
             :stretchfn (lambda (x) x (r-exp 300 300))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -36 (random 12) ) -36))
             :bw 70
             :ps 1.31
             :attackfn (lambda (x) (n-lin x 0.5 0.5))
             :releasefn (lambda (x) (n-lin x 5 5))
             :endfn (lambda (x) x 0.5)
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.002) (+ 0.003))))

;;; drone hoch!!!

(preset-play (make-instance 'eventplayer) 15 (+ 10 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool9*)) 
             :outfn (fig-out (random 13) (pick *circle-cw* *circle-ccw*))
             :transpfn (lambda (x) x 0)
             :stretchfn (lambda (x) x (r-exp 10 10))
             :wwidthfn (lambda (x) x (/ 10 16))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -12 (random 12) ) -22))
             :attackfn (lambda (x) (n-lin x 0.1 0))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.03 (random 0.03)) (+ 0.1 (random 0.2)))))

(preset-play (make-instance 'eventplayer) 15 (+ 10 (random 8))
             :lsamplefn (lambda (x) x (r-elt *pool9*)) 
             :outfn (fig-out (random 13) (pick *circle-cw* *circle-ccw*))
             :transpfn (lambda (x) x 0)
             :stretchfn (lambda (x) x (r-exp 10 10))
             :wwidthfn (lambda (x) x (/ 10 16))
             :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -12 (random 12) ) -22))
             :attackfn (lambda (x) (n-lin x 0.1 0))
             :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.03 (random 0.03)) (+ 0.1 (random 2.0)))))

;; holztest?

(preset-play (make-instance 'eventplayer) 0 1
             :g1 *pool0*
             :outfn #'global-out
             :transpfn (lambda (x) (n-lin x (+ (random 5.0) -13) 30))
             :amp (+ (random 6) -6)
             :dtimefn (lambda (x) x 3))





(preset-play (make-instance 'eventplayer) 99 (+ 1 (random 5))
             :lsamplefn (r-elt *pool11*)
             :dtimefn (r-exp 0.08 0.3)
             :outfn (fig12-out 0 *rline*))


(preset-play (aref *players* 0) 99 (+ 1 (random 5))
             :lsamplefn (r-elt *pool11*)
             :dtimefn (r-exp 0.08 0.3)
             :outfn (fig12-out 0 *rline*))

(elt pool (mod (incf buf-id) len))

(get-buffer-file (elt *pool16* 0))

(let* ((buf-id -1)
       (pool *pool13*)
       (len (length pool)))
  (preset-play (aref *players* 0) 98 (/ len 2)
               :lsamplefn (lambda () (prog1
                                   (elt pool (mod (incf buf-id) len))
                                 (format t "~a~%" (get-buffer-file (elt pool (mod buf-id len))))))
               :dtimefn 0.5
               :outfn (fig12-out 0 *rline*)))

(untrace)




(apply #'play-buffer-stretch-env-pan-out (cddr params))
(get-buffer-file (elt *pool16* 10))

(collect-argvals 0.3 15 (aref *presets* 1) :g1 *pool0* :g2 (lambda (x) (interp x 0 -12 0.5 0 1 -12)))


(funcall (getf args :g2) x)

(getf args :g1)
(funcall (getf args :g2) x)

(preset-play (aref *players* 4) 0 0.4)

(untrace)

(preset-play (aref *players* 4) 12 5)

(preset-play (aref *players* 4) -1 nil)

(preset-play (aref *players* 5) 2 (+ 5 (random 4)))
(preset-play (aref *players* 5) 1 (+ 5 (random 4)))

(incudine-plot:plot (aref *buffers* 0))

(preset-play (aref *players* 0) -1 nil)

(preset-play (aref *players* 0) 8 2)

(preset-play (aref *players* 0) -1 10)
(preset-play (aref *players* 1) 0 10)
(BIG-ORCHESTRA::COLLECT-ARGVALS 1.0 #S(BIG-ORCHESTRA::PRESET :DTIME #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :DTIME-DEV #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :DUR #S(BIG-ORCHESTRA::ENV :START 1.1 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :DUR-DEV #S(BIG-ORCHESTRA::ENV :START 1.2 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :TRANSP #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 1 :RELEASE 0 :TYPE :LIN) :TRANSP-DEV #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 1 :RELEASE 0 :TYPE :LIN) :STRETCH #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :STRETCH-DEV #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 0 :RELEASE 0 :TYPE :EXP) :WSIZE #S(BIG-ORCHESTRA::ENV :START 123 :DELTA 1 :ATTACK 0 :RELEASE 0 :TYPE :EXP) :WSIZE-DEV #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 1 :RELEASE 0 :TYPE :EXP) :AMP #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 1 :RELEASE 0 :TYPE :LIN) :AMP-DEV #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 0 :RELEASE 0 :TYPE :LIN) :INNER-ATTACK #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 0 :RELEASE 0 :TYPE :LIN) :INNER-ATTACK-DEV #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 0 :RELEASE 0 :TYPE :EXP) :INNER-RELEASE #S(BIG-ORCHESTRA::ENV :START 0.01 :DELTA 0 :ATTACK 0 :RELEASE 0 :TYPE :LIN) :INNER-RELEASE-DEV #S(BIG-ORCHESTRA::ENV :START 1 :DELTA 1 :ATTACK 0 :RELEASE 0 :TYPE :EXP) :CHAN 0 :CHAN-DEV #S(BIG-ORCHESTRA::ENV :START 0 :DELTA 0 :ATTACK 0 :RELEASE 0 :TYPE :LIN)))

(OF-INCUDINE-DSPS:PLAY-BUFFER-STRETCH-ENV-OUT
:BUFFER (aref *buffers* 0) :AMP 0.0 :TRANSP 1.0 :START 0 :END 1.0 :STRETCH 10 :WWIDTH 123 :ATTACK 0 :RELEASE 0.0 :OUT 0)

#<incudine:buffer :frames 7528 :channels 1 :sr 44100.0>

*buffers*
(of-incudine-dsps:play-buffer-stretch-env-out
 :buffer (aref *buffers* 2)
 :amp -0.8881397
 :transp 0
 :start 0
 :end 0.1
 :stretch 60.0
 :wwidth 40
 :attack 1
 :release 1
 :out 0)

(* 30 0.2)

(untrace)
(get-env-y 0 (make-env :start 1.2 :delta 1 :attack 0 :release 0 :type :exp))

(interp)

;;; (vug:rand :weibull)

(new weighting :of '(1 1 1 2 3))

(funcall (song-fn donnerblech))


(in-package :scratch)
(make-asr attack ampl release)

(defparameter env1 (make-asr 1 1 2 :release-node -1))

(defparameter env1
  (make-envelope (list 0 1 1 0) (list 1 2 1)
                 :curve (list 4 0 -4) :base nil :release-node -1
                 :restart-level nil :real-time-p (allow-rt-memory-p)))

(describe env1)

(dsp! env-test ((env envelope) attack amp release dur)
  (stereo (* (envelope (make-envelope (list 0 amp amp 0) (list attack (- 1 attack release) release)
                                      :curve (list 4 1 -4) :base nil :release-node -1
                                      :restart-level nil :real-time-p (allow-rt-memory-p))
                       1 dur #'free)
             (white-noise amp))))



(- 3 1 1)

(dsp! env-test (attack amp release dur)
  (stereo (* (envelope (reduce-warnings
                         (make-envelope
                           (list 0 amp amp 0)
                           (list attack (- dur attack release) release)
                           :curve '(3 0 -3)))
                       1 1 #'free)
             (white-noise amp))))

(env-test 1 0.3 1 3 :id 1)

(env-test 1 0.3 1 2 :id 1)

(set-control 1 :gate 0)

;;; (aref *players* 0)

;;(aref *players* 0)

;; (start (aref *players* 0))

(:start 1 :delta 1 :attack 1 :release 0 :type :exp)


(loop for buffer in (length))

(untrace)

(trace of-incudine-dsps:play-buffer-stretch-env-out)


#| (loop for x in `(:p1 0
                 :p2 0
                 :p3 0
                 :p4 0
                 :lsamplefn (r-elt *buffers*)
                 :ampfn (n-exp 0 1 2)
                 :transpfn (r-exp 1 2)
                 :startfn 0
                 :endfn 0
                 :stretchfn (r-exp 1 1)
                 :wwidthfn 123
                 :attackfn 0
                 :releasefn 0.01
                 :outfn 0) by #'cddr
      collect x)
|#

#<incudine:buffer :frames 3925 :channels 1 :sr 48000.0>

(:buffer 1
 :amp -6 :transp -11.047948 :start 0 :end 0 :stretch 1.0 :wwidth 123 :attack 0 :release 0.01
 :pan 0.5 :out1 0 :out2 1)

(loop for x below 13 collect (/ (round (* 1000 (float (/ x 12)))) 1000.0))

(0.0 0.083 0.167 0.25 0.333 0.417 0.5 0.583 0.667 0.75 0.833 0.917 1.0)

(0.0 0.08 0.17 0.25 0.33 0.42 0.5 0.58 0.67 0.75 0.83 0.92 1.0)


(interpl)


