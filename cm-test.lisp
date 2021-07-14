;;; 
;;; cm-test.lisp
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


(in-package :cm)

(use-package :big-orchestra)
(use-package :orm-utils)

(big-orchestra::preset-play
         (make-instance 'big-orchestra::eventplayer) 15 (+ 10 (random 8))
                     :bufferfn (lambda (x) x (big-orchestra::r-elt big-orchestra::*pool12*)) 
                     :outfn (big-orchestra::fig12-out (random 9) big-orchestra::*circle-cw*)
                     :transpfn (lambda (x) x (ou:r-lin (+ (random 5.0) 10) -50))
                     :ampfn (lambda (x) (ou:n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
                     :dtimefn (lambda (x) (ou:n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))

(sprout
 (rt-proc
   (loop
     for dur = (+ 2 (random 3))
     for count below 3
     do (progn
          (preset-play
           (make-instance 'eventplayer) 15 dur
           :bufferfn (lambda (x) x (r-elt big-orchestra::*pool12*)) 
           :outfn (fig12-out (random 9) big-orchestra::*circle-cw*)
           :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -50))
           :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
           :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))
          (rt-wait (+ dur 1))))))


(sprout
 (process repeat 3
          for dur = 4
   do 
   wait 5))



(defun teppichtrampeln (dur)
  (let ((params
          (list :bufferfn (lambda (x) x (r-elt big-orchestra::*pool12*)) 
                :outfn (fig12-out (random 9) big-orchestra::*circle-cw*)
                :transpfn (lambda (x) x (r-lin (+ (random 5.0) 10) -20))
                :ampfn (lambda (x) (n-lin (interp x 0 1 0.1 0 1 1) (- -6 (random 12) ) -32))
                :dtimefn (lambda (x) (n-exp (interp x 0 0 1 1) (+ 0.02 (random 0.02)) (+ 1.5 (random 2.0)))))))
    (apply #'preset-play
         (make-instance 'eventplayer) 15 dur params)))


(defun play-song (time repeat)
  (let ((dur (+ 5 (random 10))))
    (format t "playsong: ~a, " repeat)
    (teppichtrampeln dur)
    (if (> repeat 1)
        (at (+ time dur) #'play-song (+ time dur) (decf repeat)))))

(teppichtrampeln 20)

(play-song (now) 10)

(cm:rts)
(sprout (process repeat 2 do (funcall (lambda () (teppichtrampeln 10))) wait 10))

(loop
  for x below 10
  do (let ((dur (+ 5 (random 10))))
       (teppichtrampeln dur)
       (sleep dur)
       (princ "done")))
