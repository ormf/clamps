;;; 
;;; kurs-beispiel.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defun distributed-play (params)
  (if *debug* (format t "~&~a" params))
  (apply #'play-buffer-stretch-env-pan-out* params)
;;;  (apply #'send-to-remote params)
  )
(init-poolplayer)

(defun my-out (x)
  x
  (let ((ch (random 8))) (values ch ch (random 1.0))))


(play-buffer-stretch-env-pan-out* (elt *pool0* 0) :amp -6 :pan 0.5)

(at (now) #'play-buffer-stretch-env-pan-out* (elt *pool0* 0) :pan 0.5)

(at (+ (now) 1) #'play-buffer-stretch-env-pan-out* (elt *pool0* 0) :amp -6 :pan 0.5)

(defun play ()
  (let ((next (+ (now) 1)))
    (play-buffer-stretch-env-pan-out* (elt *pool0* 0) :amp -6 :pan 0.5)
    (at next #'play)))

(ql:quickload "cm")

(defun play ()
  (let ((next (+ (now) (cm::pick 1 0.5 0.25 0.25 0.25))))
    (play-buffer-stretch-env-pan-out* (elt *pool0* 0) :amp -6 :pan 0.5)
    (at next #'play)))

(play)

(defparameter holz-ex-01
  (make-song
   :name "holz-ex-01"
   :durfn (lambda () (r-exp 9 12))
   :afterfn (lambda () (r-exp 3 5))
   :beforefn (lambda () (r-exp 3 5))
   :playfn
   (lambda (dur)
     (let* ((params (list
                     :g1 *pool0*
                     :outfn #'my-out
                     :dtimefn (lambda (x) (r-exp (n-exp x 0.03 0.3) (n-exp x 0.03 6)))))
            (repeat (+ 9 (random 4)))
            (players (loop for i below repeat collect (make-eventplayer))))
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

(play-song holz-ex-01)

(funcall (song-durfn holz-ex-01))

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
              :outfn (fig5-out 0 (pick *line* *rline*))
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

(defparameter holzregen-ex-02
    (make-song
     :name "holzregen-ex-02"
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

(play-song holzregen-ex-02)

(play-song holzwirbel-01)


(defparameter kurs-ex-03
    (make-song
     :name "kurs-ex-03"
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

(play-song kurs-ex-03)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'global-out
          :dtimefn (lambda (x) (r-exp 0.02 (interp x 0 0.03 0.5 0.2 0.6 0.03 0.8 0.03 1 0.2)))
          :ampfn (lambda (x) (interp x 0 -10 0.1 0 0.7 0 1 -10))
          :g3 (lambda (x) (r-lin 10 (interp x 0 10 1 -10)))
          :g2 (lambda (x) (interp x 0 -20 0.3 -26 0.5 -26 0.6 -20 1 -60))))
 10)

((:dtime :dtimefn)
 (:lsample :lsamplefn)
 (:amp :ampfn)
 (:transp :transpfn)
 (:start :startfn)
 (:end :endfn)
 (:stretch :stretchfn)
 (:wwidth :wwidthfn)
 (:attack :attackfn)
 (:release :releasefn))

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'(lambda (x) (values 0 1 .50))
          :transpfn (lambda (x) x (random 6.0))
          :ampfn (lambda (x) x (random 12))
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) x 0.1)))
 3)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'(lambda (x) (let ((out (round (* x 8))))
                            (values out out 0)))
          :transpfn (lambda (x) x 0)
          :ampfn (lambda (x) x 0)
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) x 0.1)))
 6)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'(lambda (x) (let ((out (round (* x 8))))
                            (values out out 0)))
          :transpfn (lambda (x) x (+ 10 (random 20)))
          :ampfn (lambda (x) x 0)
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) (interp x 0 0.1 1 1 :base 12))))
 (+ 8 (random 1)))


(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'my-out
          :transpfn (lambda (x) x 0)
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) x 0.1)))
 10)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'my-out
          :transpfn (lambda (x) x 0)
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) (interp x 0 0.03 0.5 0.2 0.6 0.03 0.8 0.03 1 0.2))))
 18)

(funcall
 (playfn 0
         (list
          :g1 *pool1*
          :outfn (fig5-out 0 (pick *line* *rline*))
          :dtimefn (lambda (x) (interp x 0 0.03 0.5 0.2 0.6 0.03 0.8 0.03 1 0.2))
          :ampfn (lambda (x) x 0)
          :transpfn (lambda (x) x 0)))
 (+ 3 (random 10)))


(funcall
 (playfn 0
         (list
          :g1 *pool21*
          :outfn #'my-out
          :transpfn (lambda (x) x (r-lin 0 10))
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x (r-exp 1 2))
          :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7))))
 10)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'my-out
          :transpfn (lambda (x) x 0)
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) x 0.1)))
 10)

(funcall
 (playfn 0
         (list
          :g1 *pool0*
          :outfn #'my-out
          :transpfn (lambda (x) x 0)
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x 1)
          :dtimefn (lambda (x) x 0.1)))
 10)

(funcall
 (playfn 0
         (list
          :g1 *pool1*
          :outfn #'global-out
          :transpfn (lambda (x) x (r-lin 0 10))
          :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
          :stretchfn (lambda (x) x (r-exp 1 2))
          :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7))))
 10)

(funcall
 (playfn 80
         (list
          :g1 *pool1*
          :outfn (fig5-out 0 (pick *line* *rline*))
          :dtimefn (lambda (x) (r-exp 0.02 (interp x 0 0.03 0.5 0.2 0.6 0.03 0.8 0.03 1 0.2)))
          :ampfn (lambda (x) x 6)
          :g3 (lambda (x) (r-lin 10 (interp x 0 10 1 -10)))
          :g2 (lambda (x) (interp x 0 -20 0.3 -26 0.5 -26 0.6 -20 1 -60))))
 10)

(lambda (x) x (let ((ch (random 8))) (values ch ch 0)))

(funcall
 (playfn
  0
  (list
   :g1 *pool21*
   :outfn #'my-out
   :transpfn (lambda (x) x (r-lin 0 0))
   :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 0.7 0 1 1) (- -12 (random 12)) -32))
   :stretchfn (lambda (x) x (r-exp 1 2))
   :dtimefn (get-dtime-fn 0.05 0.1 0.2 0.4 :distribution '(7))))
 14)

(funcall
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
       (recurse-fn now))))
 3)


(play-song
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

(funcall #'global-out 0)

(funcall (lambda (x) x 3) 0)

(defparameter e-guitar-01
    (make-song
     :name "e-guitar-01"
     :beforefn (lambda () (r-exp 3 10))
     :afterfn (lambda () (r-exp 3 10))
     :durfn (lambda () (r-exp 25 45))
     :playfn
))
